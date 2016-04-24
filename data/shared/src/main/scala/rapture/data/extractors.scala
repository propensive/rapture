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

package rapture.data

import rapture.core._

import scala.annotation._

import language.higherKinds

import scala.util._

case class FilterException() extends Exception("value was removed by filter")
case class NotEmptyException() extends Exception

object GeneralExtractors {
  
  def tryExtractor[Data <: DataType[Data, _ <: DataAst ], T]
      (implicit ext: Extractor[T, Data]): Extractor[Try[T], Data] { type Throws = Nothing } =
    new Extractor[Try[T], Data] {
      type Throws = Nothing
      def extract(any: Data, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Try[T], Throws] = mode.wrap {
        try ext.extract(any.$wrap(any.$normalize), any.$ast, modes.returnTry()) catch {
          case e: Exception => Failure(e)
        }
      }
    }

  def optionExtractor[Data <: DataType[Data, _ <: DataAst], T](implicit ext: Extractor[T, Data]):
      Extractor[Option[T], Data] { type Throws = Nothing } = {

    new Extractor[Option[T], Data] {
      type Throws = Nothing
      def extract(any: Data, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Option[T], Throws] = mode.wrap {
        try ext.extract(any.$wrap(any.$normalize), any.$ast, modes.returnOption()) catch {
          case e: Exception => None
        }
      }
    }
  }
  
  def noneExtractor[Data <: DataType[_, DataAst]]: Extractor[None.type, Data] { type Throws = DataGetException with NotEmptyException } =
    new Extractor[None.type, Data] {
      type Throws = DataGetException with NotEmptyException
      def extract(value: Data, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[None.type, Throws] = mode.wrap {
        val v = value.$wrap(value.$normalize)
        if(ast.isObject(v) && ast.getKeys(v).size == 0) None
        else mode.exception[None.type, NotEmptyException](NotEmptyException())
      }
    }

  def genSeqExtractor[T, Coll[_], Data <: DataType[Data, _ <: DataAst]]
      (implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]], ext: Extractor[T, Data]):
      Extractor[Coll[T], Data] { type Throws = ext.Throws } = {
    
    new Extractor[Coll[T], Data] {
      type Throws = ext.Throws
      def extract(value: Data, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Coll[T], Throws] = mode.wrap {
        mode.catching[DataGetException, Coll[T]] {
          val v = value.$wrap(value.$normalize)
          v.$ast.getArray(v.$root.value).to[List].zipWithIndex.map { case (e, i) =>
            mode.unwrap(ext.safeExtract(v.$wrap(e), v.$ast, Some(Left(i)), mode), s"($i)")
          }.to[Coll]
        }
      }
    }
  }

  def mapExtractor[K, T, Data <: DataType[Data, _ <: DataAst ]]
      (implicit ext: Extractor[T, Data], ext2: StringParser[K]): Extractor[Map[K, T], Data] { type Throws = ext.Throws with ext2.Throws } =
    new Extractor[Map[K, T], Data] {
      type Throws = ext.Throws with ext2.Throws
      def extract(value: Data, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Map[K, T], Throws] =
        mode.wrap {
          value.$ast.getObject(value.$normalize) map {
            case (k, v) => mode.unwrap(ext2.parse(k, mode)) -> mode.unwrap(ext.safeExtract(value.$wrap(v), value.$ast, Some(Right(k)), mode))
          }
        }
      }
}

object Extractor {
  implicit def floatExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Float, Data] { type Throws = ext.Throws } =
    ext.smap(_.toFloat)

  implicit def shortExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Short, Data] { type Throws = ext.Throws } =
    ext.smap(_.toShort)

  implicit def longExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Long, Data] { type Throws = ext.Throws } = ext.smap(_.toLong)

  implicit def byteExtractor[Data](implicit ext: Extractor[Double, Data]): Extractor[Byte, Data] { type Throws = ext.Throws } =
    ext.smap(_.toInt.toByte)

  implicit def anyExtractor[Data <: DataType[_, DataAst]]: Extractor[Any, Data] { type Throws = Nothing } =
    new Extractor[Any, Data] {
      type Throws = Nothing
      def extract(value: Data, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Any, Throws] =
        mode.wrap(value.$normalize)
    }
}

@implicitNotFound("cannot extract type ${T} from ${D}.")
abstract class Extractor[T, -D] extends Functor[({ type L[x] = Extractor[x, D] })#L, T] { ext =>
  type Throws <: Exception
  
  def safeExtract(any: D, ast: DataAst, prefix: Option[Either[Int, String]], mode: Mode[_ <: MethodConstraint]):
      mode.Wrap[T, Throws] = mode.wrap {
    try mode.unwrap(extract(any, ast, mode)) catch {
      case e@TypeMismatchException(_, _) => mode.exception(e)
      case e@MissingValueException() => mode.exception(e)
    }
  }
  
  def extract(any: D, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws]

  def rawMap[T2](fn: (T, Mode[_ <: MethodConstraint]) => T2): Extractor[T2, D] = new Extractor[T2, D] {
    def extract(any: D, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T2, Throws] =
      mode.wrap(fn(mode.unwrap(ext.extract(any, ast, mode)), mode.generic))
  }

  def filter(pred: T => Boolean): Extractor[T, D] { type Throws = ext.Throws with FilterException } = new Extractor[T, D] {
    type Throws = ext.Throws with FilterException
    def extract(any: D, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, Throws] = mode.wrap {
      val result = mode.unwrap(ext.extract(any, ast, mode))
      if(pred(result)) result else mode.exception[T, FilterException](FilterException())
    }
  }

  def orElse[TS >: T, T2 <: TS, D2 <: D](ext2: => Extractor[T2, D2]): Extractor[TS, D2] { type Throws = DataGetException } =
    new Extractor[TS, D2] {
      type Throws = DataGetException
      def extract(any: D2, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[TS, Throws] = mode.wrap {
        try mode.unwrap(ext.extract(any, ast, mode)) catch {
          case e: Exception => mode.unwrap(ext2.extract(any, ast, mode))
        }
      }
  }
}
