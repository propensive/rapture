/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.json

import rapture.core._
import rapture.data._

import scala.collection.mutable.{ListBuffer, HashMap}

import language.dynamics
import language.higherKinds
import language.experimental.macros

private[json] class DynamicWorkaround(json: Json) {
  def self: Json = json.selectDynamic("self")
}

trait `Json.parse` extends MethodConstraint

private[json] trait JsonDataCompanion[+Type <: JsonDataType[Type, AstType],
    AstType <: JsonAst] extends DataCompanion[Type, AstType] {

  type ParseMethodConstraint = `Json.parse`

  /** Formats the JSON object for multi-line readability. */
  private[json] def doFormat(json: Any, ln: Int, ast: AstType, pad: String = " ",
      brk: String = "\n"): String = {
    val indent = pad*ln
    json match {
      case j =>
        if(ast.isString(j)) {
          "\""+ast.getString(j).replaceAll("\\\\", "\\\\\\\\").replaceAll("\r",
              "\\\\r").replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")+"\""
        } else if(ast.isBoolean(j)) {
          if(ast.getBoolean(j)) "true" else "false"
        } else if(ast.isNumber(j)) {
          val n = ast.getDouble(j)
          if(n == n.floor) n.toInt.toString else String(n)
        } else if(ast.isArray(j)) {
          val arr = ast.getArray(j)
          if(arr.isEmpty) "[]" else List("[", arr map { v =>
            s"${indent}${pad}${doFormat(v, ln + 1, ast, pad, brk)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(ast.isObject(j)) {
          val keys = ast.getKeys(j)
          if(keys.isEmpty) "{}" else List("{", keys map { k =>
            val inner = ast.dereferenceObject(j, k)
            s"""${indent}${pad}"${k}":${pad}${doFormat(inner, ln + 1, ast, pad, brk)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(ast.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
}


private[json] object JsonDataType

private[json] trait JsonDataType[+T <: JsonDataType[T, AstType], AstType <: JsonAst]
    extends DataType[T, AstType]

object JsonBuffer extends JsonDataCompanion[JsonBuffer, JsonBufferAst] {
  
  def construct(any: MutableCell, path: Vector[Either[Int, String]])(implicit ast:
      JsonBufferAst): JsonBuffer = new JsonBuffer(any, path)
}

class JsonBuffer(val $root: MutableCell, val $path: Vector[Either[Int, String]] = Vector())
    (implicit val $ast: JsonBufferAst) extends
    JsonDataType[JsonBuffer, JsonBufferAst] with
    MutableDataType[JsonBuffer, JsonBufferAst] with DynamicData[JsonBuffer, JsonBufferAst] {

  def $wrap(any: Any, path: Vector[Either[Int, String]]): JsonBuffer =
    new JsonBuffer(MutableCell(any), path)
  
  def $deref(path: Vector[Either[Int, String]]): JsonBuffer = new JsonBuffer($root, path)
  
  def $extract(sp: Vector[Either[Int, String]]): JsonBuffer =
    if(sp.isEmpty) this else sp match {
      case Left(i) +: tail => apply(i).$extract(tail)
      case Right(e) +: tail => selectDynamic(e).$extract(tail)
    }
  
  override def toString =
    try JsonBuffer.format(this)(formatters.compact()($ast)) catch {
      case e: Exception => "undefined"
    }
}

/** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
  * pretty printer. */
object Json extends JsonDataCompanion[Json, JsonAst] with Json_1 {
  
  def construct(any: MutableCell, path: Vector[Either[Int, String]])(implicit ast:
      JsonAst): Json = new Json(any, path)

  def ast(json: Json): JsonAst = json.$ast

  def extractor[T](implicit ext: Extractor[T, Json]): Extractor[T, Json] { type Throws = ext.Throws } = ext
  def serializer[T](implicit ser: Serializer[T, Json]) = ser

  implicit def jsonCastExtractor[T: JsonCastExtractor](implicit ast: JsonAst):
      Extractor[T, JsonDataType[_, _ <: JsonAst]] =
    new Extractor[T, JsonDataType[_, _ <: JsonAst]] {
      type Throws = DataGetException
      def extract(value: JsonDataType[_, _ <: JsonAst], ast2: DataAst, mode: Mode[_]): mode.Wrap[T, DataGetException] =
        mode.wrap(ast2 match {
          case ast2: JsonAst =>
            val norm = mode.catching[DataGetException, Any](value.$normalize)
            try {
              if(ast == ast2) norm.asInstanceOf[T]
              else Json.jsonSerializer.serialize(Json.construct(MutableCell(norm),
                  Vector())(ast2)).asInstanceOf[T]
            } catch { case e: ClassCastException =>
              mode.exception[T, DataGetException](TypeMismatchException(ast.getType(norm),
                  implicitly[JsonCastExtractor[T]].dataType))
            }
          case _ => ???
        })
    }

}

/** Represents some parsed JSON. */
class Json(val $root: MutableCell, val $path: Vector[Either[Int, String]] = Vector())(implicit
    val $ast: JsonAst) extends JsonDataType[Json, JsonAst] with DynamicData[Json, JsonAst] {
  
  def $wrap(any: Any, path: Vector[Either[Int, String]]): Json =
    new Json(MutableCell(any), path)
  
  def $deref(path: Vector[Either[Int, String]]): Json = new Json($root, path)

  def $extract(sp: Vector[Either[Int, String]]): Json =
    if(sp.isEmpty) this else sp match {
      case Left(i) +: tail => apply(i).$extract(tail)
      case Right(e) +: tail => selectDynamic(e).$extract(tail)
    }
  
  override def toString =
    try Json.format(this)(formatters.compact()($ast)) catch {
      case e: Exception => "undefined"
    }
}

private[json] trait Json_1 extends Json_2 {

  implicit def optionSerializer[Type]
      (implicit ast: JsonAst, ser: Serializer[Type, Json]): Serializer[Option[Type], Json] =
    BasicJsonSerializer(_ map ser.serialize getOrElse ast.nullValue)

}

private[json] trait Json_2 extends Json_3 {

  implicit def optionExtractor[T](implicit ext: Extractor[T, Json]): Extractor[Option[T], Json] { type Throws =
      Nothing } = GeneralExtractors.optionExtractor[Json, T] 

  implicit def tryExtractor[T](implicit ext: Extractor[T, Json]): Extractor[scala.util.Try[T], Json] { type Throws =
      Nothing } = GeneralExtractors.tryExtractor[Json, T] 

  implicit def genSeqExtractor[T, Coll[_]](implicit cbf: scala.collection.generic.CanBuildFrom[Nothing, T, Coll[T]],
      ext: Extractor[T, Json]): Extractor[Coll[T], Json] { type Throws = ext.Throws } = {
   
    GeneralExtractors.genSeqExtractor[T, Coll, Json]
  }  

  implicit def mapExtractor[K, V](implicit ext: Extractor[V, Json], ext2: StringParser[K]): Extractor[Map[K, V], Json] =
    GeneralExtractors.mapExtractor[K, V, Json]

}

private[json] trait Json_3 extends Json_4 {

  case class BasicJsonSerializer[T](serialization: T => Any)
      extends Serializer[T, Json] { def serialize(t: T): Any = serialization(t) }

  implicit def jsonBufferSerializer[T](implicit ser: Serializer[T, Json]):
      Serializer[T, JsonBuffer] =
    new Serializer[T, JsonBuffer] { def serialize(t: T): Any = ser.serialize(t) }

  implicit def intSerializer(implicit ast: JsonAst): Serializer[Int, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def booleanSerializer(implicit ast: JsonAst): Serializer[Boolean, Json] =
    BasicJsonSerializer(ast fromBoolean _)

  implicit def stringSerializer(implicit ast: JsonAst): Serializer[String, Json] =
    BasicJsonSerializer(ast fromString _)

  implicit def floatSerializer(implicit ast: JsonAst): Serializer[Float, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def doubleSerializer(implicit ast: JsonAst): Serializer[Double, Json] =
    BasicJsonSerializer(ast fromDouble _)

  implicit def bigDecimalSerializer(implicit ast: JsonAst): Serializer[BigDecimal, Json] =
    BasicJsonSerializer(ast fromBigDecimal _)

  implicit def bigIntSerializer(implicit ast: JsonAst): Serializer[BigInt, Json] =
    BasicJsonSerializer(ast fromBigDecimal BigDecimal(_))

  implicit def longSerializer(implicit ast: JsonAst): Serializer[Long, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def shortSerializer(implicit ast: JsonAst): Serializer[Short, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def byteSerializer(implicit ast: JsonAst): Serializer[Byte, Json] =
    BasicJsonSerializer(ast fromDouble _.toDouble)

  implicit def nilSerializer(implicit ast: JsonAst): Serializer[Nil.type, Json] =
    BasicJsonSerializer(v => ast fromArray Nil)

  implicit def traversableSerializer[Type, Coll[T] <: Traversable[T]]
      (implicit ast: JsonAst, ser: Serializer[Type, Json]): Serializer[Coll[Type], Json] =
    BasicJsonSerializer(ast fromArray _.map(ser.serialize).to[List])

  implicit def mapSerializer[K, Type, Ast <: JsonAst]
      (implicit ast: Ast, ser: Serializer[Type, Json], ser2: StringSerializer[K]): Serializer[Map[K, Type], Json] =
    new Serializer[Map[K, Type], Json] {
      def serialize(m: Map[K, Type]) =
        ast.fromObject(m.map { case (k, v) =>
	  ser2.serialize(k) -> ser.serialize(v)
	})
    }

  implicit def directJsonSerializer[T: DirectJsonSerializer](implicit ast: JsonAst):
      Serializer[T, Json] =
    BasicJsonSerializer(obj => jsonSerializer.serialize(Json.construct(MutableCell(obj),
        Vector())(?[DirectJsonSerializer[T]].ast)))

  implicit def jsonSerializer[JsonType <: JsonDataType[JsonType, _ <: JsonAst]]
      (implicit ast: JsonAst): Serializer[JsonType, Json] =
    BasicJsonSerializer[JsonType]({ j =>
      if(j.$ast == ast) j.$normalize else ast.convert(j.$normalize, j.$ast)
    })
}

private[json] trait Json_4 extends Json_5 {

  implicit def jsonExtractor(implicit ast: JsonAst): Extractor[Json, Json] { type Throws = DataGetException } =
    new Extractor[Json, Json] {
      type Throws = DataGetException
      def extract(any: Json, dataAst: DataAst, mode: Mode[_]): mode.Wrap[Json, DataGetException] =
        mode.wrap(mode.catching[DataGetException, Json](any.$wrap(any.$normalize)))
    }
  
  implicit val stringExtractor: Extractor[String, Json] { type Throws = DataGetException } =
    new Extractor[String, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_]): mode.Wrap[String, DataGetException] =
        mode.wrap(mode.catching[DataGetException, String](any.$ast.getString(any.$normalize)))
    }

  implicit val doubleExtractor: Extractor[Double, Json] { type Throws = DataGetException } =
    new Extractor[Double, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_]): mode.Wrap[Double, Throws] =
        mode.wrap(mode.catching[DataGetException, Double](any.$ast.getDouble(any.$normalize)))
    }

  implicit val intExtractor: Extractor[Int, Json] { type Throws = DataGetException } =
    doubleExtractor.smap(_.toInt)

  implicit val booleanExtractor: Extractor[Boolean, Json] { type Throws = DataGetException } =
    new Extractor[Boolean, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_]): mode.Wrap[Boolean, DataGetException] =
        mode.wrap(any.$ast.getBoolean(any.$normalize))
    }
  
  implicit val bigDecimalExtractor: Extractor[BigDecimal, Json] { type Throws = DataGetException } =
    new Extractor[BigDecimal, Json] {
      type Throws = DataGetException
      def extract(any: Json, ast: DataAst, mode: Mode[_]): mode.Wrap[BigDecimal, DataGetException] =
        mode.wrap(any.$ast.getBigDecimal(any.$normalize))
    }
  
  implicit val bigIntExtractor: Extractor[BigInt, Json] { type Throws = DataGetException } =
    bigDecimalExtractor.smap(_.toBigInt)
}

private[json] trait Json_5 extends Json_6 {

  implicit def jsonBufferExtractor[T](implicit jsonAst: JsonAst, ext: Extractor[T, Json]):
      Extractor[T, JsonBuffer] { type Throws = ext.Throws } = new Extractor[T, JsonBuffer] {
    type Throws = ext.Throws
    def extract(any: JsonBuffer, ast: DataAst, mode: Mode[_]): mode.Wrap[T, ext.Throws] =
      ext.extract(Json.construct(MutableCell(any.$root.value), Vector()), ast, mode)
  }
  
  implicit def jsonBufferToJsonExtractor(implicit ast: JsonBufferAst): Extractor[JsonBuffer, Json] =
    new Extractor[JsonBuffer, Json] {
      type Throws = DataGetException
      def extract(any: Json, dataAst: DataAst, mode: Mode[_]): mode.Wrap[JsonBuffer, Throws] =
        mode.wrap(JsonBuffer.construct(MutableCell(Json.jsonSerializer.serialize(any)), Vector()))
    }

}

private[json] trait Json_6 extends Json_7 {
  implicit def dynamicWorkaround(j: Json) = new DynamicWorkaround(j)
}

private[json] trait Json_7 {
  implicit def jsonExtractorMacro[T <: Product, Th]: Extractor[T, Json] { type Throws = Th } =
    macro JsonMacros.jsonExtractorMacro[T, Th]

  implicit def jsonSerializerMacro[T <: Product](implicit ast: JsonAst): Serializer[T, Json] =
    macro JsonMacros.jsonSerializerMacro[T]
}

