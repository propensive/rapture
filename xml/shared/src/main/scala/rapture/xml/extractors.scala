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

package rapture.xml

import rapture.core._

import scala.util._

import language.higherKinds

case class FilterException() extends Exception("value was removed by filter")
case class NotEmptyException() extends Exception

object GeneralExtractors {

  def tryExtractor[Data <: DataType[Data, _ <: XmlAst], T](
                                                             implicit ext: Extractor[T, Data]): Extractor[Try[T], Data] { type Throws = Nothing } =
    new Extractor[Try[T], Data] {
      type Throws = Nothing
      def extract(any: Data, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Try[T], Throws] = mode.wrap {
        try ext.extract(any.$wrap(any.$normalize), any.$ast, modes.returnTry())
        catch {
          case e: Exception => Failure(e)
        }
      }
    }

  def optionExtractor[Data <: DataType[Data, _ <: XmlAst], T](
                                                                implicit ext: Extractor[T, Data]): Extractor[Option[T], Data] { type Throws = Nothing } = {

    new Extractor[Option[T], Data] {
      type Throws = Nothing
      def extract(any: Data, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Option[T], Throws] =
        mode.wrap {
          try ext.extract(any.$wrap(any.$normalize), any.$ast, modes.returnOption())
          catch {
            case e: Exception => None
          }
        }
    }
  }

  def noneExtractor[Data <: DataType[_, XmlAst]]
  : Extractor[None.type, Data] { type Throws = DataGetException with NotEmptyException } =
    new Extractor[None.type, Data] {
      type Throws = DataGetException with NotEmptyException
      def extract(value: Data, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[None.type, Throws] =
        mode.wrap {
          val v = value.$wrap(value.$normalize)
          if (ast.isObject(v) && ast.getKeys(v).size == 0) None
          else mode.exception[None.type, NotEmptyException](NotEmptyException())
        }
    }

  def genSeqExtractor[T, Coll[_], Data <: DataType[Data, _ <: XmlAst]](
                                                                         implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
                                                                         ext: Extractor[T, Data]): Extractor[Coll[T], Data] { type Throws = ext.Throws } = {

    new Extractor[Coll[T], Data] {
      type Throws = ext.Throws
      def extract(value: Data, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Coll[T], Throws] =
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

  def mapExtractor[K, T, Data <: DataType[Data, _ <: XmlAst]](
                                                                implicit ext: Extractor[T, Data],
                                                                ext2: StringParser[K]): Extractor[Map[K, T], Data] { type Throws = ext.Throws with ext2.Throws } =
    new Extractor[Map[K, T], Data] {
      type Throws = ext.Throws with ext2.Throws
      def extract(value: Data, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Map[K, T], Throws] =
        mode.wrap {
          value.$ast.getObject(value.$normalize) map {
            case (k, v) =>
              mode.unwrap(ext2.parse(k, mode)) -> mode.unwrap(
                ext.safeExtract(value.$wrap(v), value.$ast, Some(Right(k)), mode))
          }
        }
    }
}

object Extractor {
  implicit def anyExtractor[Data <: DataType[_, XmlAst]]: Extractor[Any, Data] { type Throws = Nothing } =
    new Extractor[Any, Data] {
      type Throws = Nothing
      def extract(value: Data, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Any, Throws] =
        mode.wrap(value.$normalize)
    }
}

@implicitNotFound("cannot extract type ${T} from ${D}.")
abstract class Extractor[T, -D] extends Functor[({ type L[x] = Extractor[x, D] })#L, T] { ext =>
  type Throws <: Exception

  def safeExtract(any: D,
                  ast: XmlAst,
                  prefix: Option[Either[Int, String]],
                  mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws] = mode.wrap {
    try mode.unwrap(extract(any, ast, mode))
    catch {
      case e @ TypeMismatchException(_, _) => mode.exception(e)
      case e @ MissingValueException(_) => mode.exception(e)
    }
  }

  def extract(any: D, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws]

  def rawMap[T2](fn: (T, Mode[_ <: MethodConstraint]) => T2): Extractor[T2, D] = new Extractor[T2, D] {
    def extract(any: D, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T2, Throws] =
      mode.wrap(fn(mode.unwrap(ext.extract(any, ast, mode)), mode.generic))
  }

  def filter(pred: T => Boolean): Extractor[T, D] { type Throws = ext.Throws with FilterException } =
    new Extractor[T, D] {
      type Throws = ext.Throws with FilterException
      def extract(any: D, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws] = mode.wrap {
        val result = mode.unwrap(ext.extract(any, ast, mode))
        if (pred(result)) result else mode.exception[T, FilterException](FilterException())
      }
    }

  def orElse[TS >: T, T2 <: TS, D2 <: D](
                                          ext2: => Extractor[T2, D2]): Extractor[TS, D2] { type Throws = DataGetException } =
    new Extractor[TS, D2] {
      type Throws = DataGetException
      def extract(any: D2, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[TS, Throws] = mode.wrap {
        try mode.unwrap(ext.extract(any, ast, mode))
        catch {
          case e: Exception => mode.unwrap(ext2.extract(any, ast, mode))
        }
      }
    }
}


private[xml] case class XmlCastExtractor[T](ast: XmlAst, dataType: DataTypes.DataType)

private[xml] trait Extractors extends Extractors_1 {

  implicit def optionExtractor[T](
      implicit ext: Extractor[T, Xml]): Extractor[Option[T], Xml] { type Throws = Nothing } =
    GeneralExtractors.optionExtractor[Xml, T]

  implicit def tryExtractor[T](implicit ext: Extractor[T, Xml]): Extractor[Try[T], Xml] { type Throws = Nothing } =
    GeneralExtractors.tryExtractor[Xml, T]

  implicit def genSeqExtractor[T, Coll[_]](
      implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
      ext: Extractor[T, Xml]): Extractor[Coll[T], Xml] { type Throws = ext.Throws } = {

    GeneralExtractors.genSeqExtractor[T, Coll, Xml]
  }

  implicit def xmlExtractor(implicit ast: XmlAst): Extractor[Xml, Xml] { type Throws = DataGetException } =
    new Extractor[Xml, Xml] {
      type Throws = DataGetException
      def extract(any: Xml, dataAst: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Xml, DataGetException] =
        mode.wrap(mode.catching[DataGetException, Xml](any.$wrap(any.$normalize)))
    }

}

private[xml] trait Extractors_1 {

  implicit def stringableExtractors[T](implicit ext: StringParser[T]): Extractor[T, Xml] {
    type Throws = DataGetException with ext.Throws
  } = new Extractor[T, Xml] {

    type Throws = DataGetException with ext.Throws

    def extract(any: Xml,
                ast: XmlAst,
                mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, DataGetException with ext.Throws] = mode.wrap {
      val value: String = mode.catching[DataGetException, String](any.$ast.getString(any.$normalize))
      mode.unwrap(ext.parse(value, mode))
    }
  }

  implicit def xmlBufferExtractor[T](implicit xmlAst: XmlAst,
                                     ext: Extractor[T, Xml]): Extractor[T, XmlBuffer] { type Throws = ext.Throws } =
    new Extractor[T, XmlBuffer] {
      type Throws = ext.Throws
      def extract(any: XmlBuffer, ast: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, ext.Throws] =
        ext.extract(Xml.construct(MutableCell(any.$root.value), Vector()), ast, mode)
    }

  implicit def xmlBufferToXmlExtractor(implicit ast: XmlBufferAst): Extractor[XmlBuffer, Xml] =
    new Extractor[XmlBuffer, Xml] {
      type Throws = DataGetException
      def extract(any: Xml, dataAst: XmlAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[XmlBuffer, Throws] =
        mode.wrap(XmlBuffer.construct(MutableCell(XmlDataType.xmlSerializer.serialize(any)), Vector()))
    }

}
