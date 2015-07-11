/******************************************************************************************************************\
* Rapture JSON, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
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

private[json] trait Json_2 {
  implicit def jsonExtractorMacro[T <: Product, Th]: Extractor[T, Json] { type Throws = Th } =
    macro JsonMacros.jsonExtractorMacro[T, Th]

  implicit def jsonSerializerMacro[T <: Product](implicit ast: JsonAst): Serializer[T, Json] =
    macro JsonMacros.jsonSerializerMacro[T]
}

private[json] trait Json_1 extends Json_2 {
  implicit def dynamicWorkaround(j: Json) = new DynamicWorkaround(j)
}

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


private[json] object JsonDataType extends Extractors with Serializers

private[json] trait JsonDataType[+T <: JsonDataType[T, AstType], AstType <: JsonAst]
    extends DataType[T, AstType]

object JsonBuffer extends JsonDataCompanion[JsonBuffer, JsonBufferAst] {
  
  def construct(any: MutableCell, path: Vector[Either[Int, String]])(implicit ast:
      JsonBufferAst): JsonBuffer = new JsonBuffer(any, path)
}

/** Companion object to the `Json` type, providing factory and extractor methods, and a JSON
  * pretty printer. */
object Json extends JsonDataCompanion[Json, JsonAst] with Json_1 {
  
  def construct(any: MutableCell, path: Vector[Either[Int, String]])(implicit ast:
      JsonAst): Json = new Json(any, path)

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
              else JsonDataType.jsonSerializer.serialize(Json.construct(MutableCell(norm),
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
