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
import rapture.data._

import scala.util._

import language.higherKinds

private[json] case class JsonCastExtractor[T](ast: JsonAst, dataType: DataTypes.DataType)

private[json] trait Extractors extends Extractors_1 {

  implicit def optionExtractor[T](implicit ext: Extractor[T, Json]): Extractor[Option[T], Json] { type Throws =
      Nothing } = GeneralExtractors.optionExtractor[Json, T] 

  implicit def tryExtractor[T](implicit ext: Extractor[T, Json]): Extractor[Try[T], Json] { type Throws =
      Nothing } = GeneralExtractors.tryExtractor[Json, T] 

  implicit def genSeqExtractor[T, Coll[_]](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
      ext: Extractor[T, Json]): Extractor[Coll[T], Json] { type Throws = ext.Throws } = {
   
    GeneralExtractors.genSeqExtractor[T, Coll, Json]
  }  

  implicit def mapExtractor[K, V](implicit ext: Extractor[V, Json], ext2: StringParser[K]): Extractor[Map[K, V], Json] =
    GeneralExtractors.mapExtractor[K, V, Json]

}

private[json] trait Extractors_1 extends Extractors_2 {

  implicit def jsonExtractor(implicit ast: JsonAst): Extractor[Json, Json] { type Throws = DataGetException } =
    new Extractor[Json, Json] {
      type Throws = DataGetException
      def extract(any: Json, dataAst: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Json, DataGetException] =
        mode.wrap(mode.catching[DataGetException, Json](any.$wrap(any.$normalize)))
    }
  
  implicit val stringExtractor: Extractor[String, Json] { type Throws = DataGetException } =
    new Extractor[String, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[String, DataGetException] =
        mode.wrap(mode.catching[DataGetException, String](any.$ast.getString(any.$normalize)))
    }

  implicit val doubleExtractor: Extractor[Double, Json] { type Throws = DataGetException } =
    new Extractor[Double, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Double, Throws] =
        mode.wrap(mode.catching[DataGetException, Double](any.$ast.getDouble(any.$normalize)))
    }

  implicit val intExtractor: Extractor[Int, Json] { type Throws = DataGetException } =
    doubleExtractor.smap(_.toInt)

  implicit val booleanExtractor: Extractor[Boolean, Json] { type Throws = DataGetException } =
    new Extractor[Boolean, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Boolean, DataGetException] =
        mode.wrap(any.$ast.getBoolean(any.$normalize))
    }
  
  implicit val bigDecimalExtractor: Extractor[BigDecimal, Json] { type Throws = DataGetException } =
    new Extractor[BigDecimal, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[BigDecimal, DataGetException] =
        mode.wrap(any.$ast.getBigDecimal(any.$normalize))
    }
  
  implicit val bigIntExtractor: Extractor[BigInt, Json] { type Throws = DataGetException } =
    bigDecimalExtractor.smap(_.toBigInt)
}

private[json] trait Extractors_2 {

  implicit def jsonBufferExtractor[T](implicit jsonAst: JsonAst, ext: Extractor[T, Json]):
      Extractor[T, JsonBuffer] { type Throws = ext.Throws } = new Extractor[T, JsonBuffer] {
    type Throws = ext.Throws
    def extract(any: JsonBuffer, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[T, ext.Throws] =
      ext.extract(Json.construct(MutableCell(any.$root.value), Vector()), ast, mode)
  }
  
  implicit def jsonBufferToJsonExtractor(implicit ast: JsonBufferAst): Extractor[JsonBuffer, Json] =
    new Extractor[JsonBuffer, Json] {
      type Throws = DataGetException
      def extract(any: Json, dataAst: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[JsonBuffer, Throws] =
        mode.wrap(JsonBuffer.construct(MutableCell(JsonDataType.jsonSerializer.serialize(any)), Vector()))
    }

  implicit def generalStringExtractor[S](implicit parser: StringParser[S]): Extractor[S, Json] {
      type Throws = DataGetException with parser.Throws } = new Extractor[S, Json] {
    type Throws = DataGetException with parser.Throws
    def extract(any: Json, ast: DataAst, mode: Mode[_ <: MethodConstraint]): mode.Wrap[S, DataGetException with parser.Throws] =
      mode.wrap(mode.unwrap(parser.parse(any.$ast.getString(any.$normalize), mode)))
  }
}
