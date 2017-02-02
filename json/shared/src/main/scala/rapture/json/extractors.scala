/*
  Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.

  The primary distribution site is
  
    http://rapture.io/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  
    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is
  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and limitations under the License.
 */

package rapture.json

import rapture.core._

import scala.util._
import language.higherKinds

case class FilterException() extends Exception("value was removed by filter")
case class NotEmptyException() extends Exception

@implicitNotFound("cannot extract type ${T} from ${D}.")
abstract class Extractor[T, -D] extends Functor[({ type L[x] = Extractor[x, D] })#L, T] { ext =>
  type Throws <: Exception

  def safeExtract(any: D,
                  ast: JsonAst,
                  prefix: Option[Either[Int, String]],
                  mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws] = mode.wrap[T, Throws] {
    try mode.unwrap(extract(any, ast, mode))
    catch {
      case e @ TypeMismatchException(_, _) => mode.exception(e)
      case e @ MissingValueException(_) => mode.exception(e)
    }
  }

  def extract(any: D, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws]

  def rawMap[T2](fn: (T, Mode[_ <: MethodConstraint]) => T2): Extractor[T2, D] = new Extractor[T2, D] {
    def extract(any: D, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T2, Throws] =
      mode.wrap(fn(mode.unwrap(ext.extract(any, ast, mode)), mode.generic))
  }

  def filter(pred: T => Boolean): Extractor[T, D] { type Throws = ext.Throws with FilterException } =
    new Extractor[T, D] {
      type Throws = ext.Throws with FilterException
      def extract(any: D, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws] = mode.wrap {
        val result = mode.unwrap(ext.extract(any, ast, mode))
        if (pred(result)) result else mode.exception[T, FilterException](FilterException())
      }
    }

  def orElse[TS >: T, T2 <: TS, D2 <: D](ext2: => Extractor[T2, D2]): Extractor[TS, D2] { type Throws = DataGetException } =
    new Extractor[TS, D2] {
      type Throws = DataGetException
      def extract(any: D2, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[TS, Throws] = mode.wrap {
        try mode.unwrap(ext.extract(any, ast, mode))
        catch {
          case e: Exception => mode.unwrap(ext2.extract(any, ast, mode))
        }
      }
    }
}

object Extractor {
  implicit def anyExtractor[Data <: DataType[_, JsonAst]]: Extractor[Any, Data] { type Throws = Nothing } =
    new Extractor[Any, Data] {
      type Throws = Nothing
      def extract(value: Data, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Any, Throws] =
        mode.wrap(value.$normalize)
    }
}

object GeneralExtractors {

  def tryExtractor[Data <: DataType[Data, _ <: JsonAst], T](
                                                             implicit ext: Extractor[T, Data]): Extractor[Try[T], Data] { type Throws = Nothing } =
    new Extractor[Try[T], Data] {
      type Throws = Nothing
      def extract(any: Data, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Try[T], Throws] = mode.wrap {
        try ext.extract(any.$wrap(any.$normalize), any.$ast, modes.returnTry())
        catch {
          case e: Exception => Failure(e)
        }
      }
    }

  def optionExtractor[Data <: DataType[Data, _ <: JsonAst], T](
                                                                implicit ext: Extractor[T, Data]): Extractor[Option[T], Data] { type Throws = Nothing } = {

    new Extractor[Option[T], Data] {
      type Throws = Nothing
      def extract(any: Data, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Option[T], Throws] =
        mode.wrap {
          try ext.extract(any.$wrap(any.$normalize), any.$ast, modes.returnOption())
          catch {
            case e: Exception => None
          }
        }
    }
  }

  def noneExtractor[Data <: DataType[_, JsonAst]]
  : Extractor[None.type, Data] { type Throws = DataGetException with NotEmptyException } =
    new Extractor[None.type, Data] {
      type Throws = DataGetException with NotEmptyException
      def extract(value: Data, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[None.type, Throws] =
        mode.wrap {
          val v = value.$wrap(value.$normalize)
          if (ast.isObject(v) && ast.getKeys(v).isEmpty) None
          else mode.exception[None.type, NotEmptyException](NotEmptyException())
        }
    }

  def genSeqExtractor[T, Coll[_], Data <: DataType[Data, _ <: JsonAst]](
                                                                         implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
                                                                         ext: Extractor[T, Data]): Extractor[Coll[T], Data] { type Throws = ext.Throws } = {

    new Extractor[Coll[T], Data] {
      type Throws = ext.Throws
      def extract(value: Data, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Coll[T], Throws] =
        mode.wrap {
          mode.catching[DataGetException, Coll[T]] {
            val v = value.$wrap(value.$normalize)
            v.$ast
              .getArray(v.$root.value)
              .to[List]
              .zipWithIndex
              .map {
                case (e, i) =>
                  mode.unwrap(ext.safeExtract(v.$wrap(e), v.$ast, Some(Left(i)), mode), s"($i)")
              }
              .to[Coll]
          }
        }
    }
  }

  def mapExtractor[K, T, Data <: DataType[Data, _ <: JsonAst]](
                                                                implicit ext: Extractor[T, Data],
                                                                ext2: StringParser[K]): Extractor[Map[K, T], Data] { type Throws = ext.Throws with ext2.Throws } =
    new Extractor[Map[K, T], Data] {
      type Throws = ext.Throws with ext2.Throws
      def extract(value: Data, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Map[K, T], Throws] =
        mode.wrap {
          value.$ast.getObject(value.$normalize) map {
            case (k, v) =>
              mode.unwrap(ext2.parse(k, mode)) -> mode.unwrap(
                ext.safeExtract(value.$wrap(v), value.$ast, Some(Right(k)), mode))
          }
        }
    }
}


private[json] case class JsonCastExtractor[T](ast: JsonAst, dataType: DataTypes.DataType)

private[json] trait Extractors extends Extractors_1 {

  implicit def optionExtractor[T](
      implicit ext: Extractor[T, Json]): Extractor[Option[T], Json] { type Throws = Nothing } =
    GeneralExtractors.optionExtractor[Json, T]

  implicit def tryExtractor[T](implicit ext: Extractor[T, Json]): Extractor[Try[T], Json] { type Throws = Nothing } =
    GeneralExtractors.tryExtractor[Json, T]

  implicit def genSeqExtractor[T, Coll[_]](
      implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
      ext: Extractor[T, Json]): Extractor[Coll[T], Json] { type Throws = ext.Throws } = {

    GeneralExtractors.genSeqExtractor[T, Coll, Json]
  }

  implicit def mapExtractor[K, V](implicit ext: Extractor[V, Json],
                                  ext2: StringParser[K]): Extractor[Map[K, V], Json] =
    GeneralExtractors.mapExtractor[K, V, Json]

}

private[json] trait Extractors_1 extends Extractors_2 {

  implicit def jsonExtractor(implicit ast: JsonAst): Extractor[Json, Json] { type Throws = DataGetException } =
    new Extractor[Json, Json] {
      type Throws = DataGetException
      def extract(any: Json, dataAst: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Json, DataGetException] =
        mode.wrap(mode.catching[DataGetException, Json](any.$wrap(any.$normalize)))
    }

  implicit val stringExtractor: Extractor[String, Json] { type Throws = DataGetException } =
    new Extractor[String, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[String, DataGetException] =
        mode.wrap(mode.catching[DataGetException, String](any.$ast.getString(any.$normalize)))
    }

  implicit val nullExtractor: Extractor[Null, Json] { type Throws = DataGetException } = new Extractor[Null, Json] {
    type Throws = DataGetException
    override def extract(any: Json, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Null, this.Throws] = {
      mode.wrap(
          mode.catching[DataGetException, Null](
              if (any.$ast.isNull(any.$normalize)) null
              else throw TypeMismatchException(any.$ast.getType(any.$normalize), DataTypes.Null)
          ))
    }
  }

  implicit val doubleExtractor: Extractor[Double, Json] { type Throws = DataGetException } =
    new Extractor[Double, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Double, Throws] =
        mode.wrap(mode.catching[DataGetException, Double](any.$ast.getDouble(any.$normalize)))
    }

  implicit val intExtractor: Extractor[Int, Json] { type Throws = DataGetException } = doubleExtractor.smap(_.toInt)

  implicit val booleanExtractor: Extractor[Boolean, Json] { type Throws = DataGetException } =
    new Extractor[Boolean, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Boolean, DataGetException] =
        mode.wrap(any.$ast.getBoolean(any.$normalize))
    }

  implicit val bigDecimalExtractor: Extractor[BigDecimal, Json] { type Throws = DataGetException } =
    new Extractor[BigDecimal, Json] {
      type Throws = DataGetException
      def extract(any: Json,
                  ast: JsonAst,
                  mode: Mode[_ <: MethodConstraint]): mode.Wrap[BigDecimal, DataGetException] =
        mode.wrap(any.$ast.getBigDecimal(any.$normalize))
    }

  implicit val bigIntExtractor: Extractor[BigInt, Json] { type Throws = DataGetException } =
    bigDecimalExtractor.smap(_.toBigInt)

  implicit val longExtractor: Extractor[Long, Json] { type Throws = DataGetException } = bigIntExtractor.smap(_.toLong)

  implicit val byteExtractor: Extractor[Byte, Json] { type Throws = DataGetException } = intExtractor.smap(_.toByte)

  implicit val floatExtractor: Extractor[Float, Json] { type Throws = DataGetException } =
    doubleExtractor.smap(_.toFloat)

  implicit val shortExtractor: Extractor[Short, Json] { type Throws = DataGetException } = intExtractor.smap(_.toShort)

}

private[json] trait Extractors_2 {

  implicit def jsonBufferExtractor[T](implicit jsonAst: JsonAst,
                                      ext: Extractor[T, Json]): Extractor[T, JsonBuffer] { type Throws = ext.Throws } =
    new Extractor[T, JsonBuffer] {
      type Throws = ext.Throws
      def extract(any: JsonBuffer, ast: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, ext.Throws] =
        ext.extract(Json.construct(MutableCell(any.$root.value), Vector()), ast, mode)
    }

  implicit def jsonBufferToJsonExtractor(implicit ast: JsonBufferAst): Extractor[JsonBuffer, Json] =
    new Extractor[JsonBuffer, Json] {
      type Throws = DataGetException
      def extract(any: Json, dataAst: JsonAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[JsonBuffer, Throws] =
        mode.wrap(JsonBuffer.construct(MutableCell(JsonDataType.jsonSerializer.serialize(any)), Vector()))
    }

  implicit def generalStringExtractor[S](implicit parser: StringParser[S]): Extractor[S, Json] {
    type Throws = DataGetException with parser.Throws
  } = new Extractor[S, Json] {
    type Throws = DataGetException with parser.Throws
    def extract(any: Json,
                ast: JsonAst,
                mode: Mode[_ <: MethodConstraint]): mode.Wrap[S, DataGetException with parser.Throws] =
      mode.wrap(mode.unwrap(parser.parse(any.$ast.getString(any.$normalize), mode)))
  }
}
