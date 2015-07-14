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

import rapture.base._
import rapture.core._
import rapture.data._

import language.experimental.macros
import scala.reflect.macros._

private[json] object JsonDataMacros extends DataContextMacros[Json, JsonAst] {
  
  def companion(c: BlackboxContext): c.Expr[DataCompanion[Json, JsonAst]] = c.universe.reify(Json)

  def parseSource(s: List[String]) = try {
    JsonValidator.validate(s)
    None
  } catch {
    case JsonValidator.ValidationException(strNo, pos, expected, found) =>
      val f = if(found == '\u0000') "end of input" else s"'$found'"
      Some((strNo, pos, s"Failed to parse Json literal: Expected $expected, but found $f"))
    case JsonValidator.DuplicateKeyException(strNo, pos, key) =>
      Some((strNo, pos, s"""Duplicate key found in Json literal: "$key""""))
  }
  
  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[Json]]*)
      (parser: c.Expr[Parser[String, JsonAst]]): c.Expr[Json] =
    super.contextMacro(c)(exprs: _*)(parser)

}

private[json] object JsonBufferDataMacros extends DataContextMacros[JsonBuffer, JsonBufferAst] {
  
  def companion(c: BlackboxContext): c.Expr[DataCompanion[JsonBuffer, JsonBufferAst]] =
    c.universe.reify(JsonBuffer)

  def parseSource(s: List[String]) = try {
    JsonValidator.validate(s)
    None
  } catch {
    case JsonValidator.ValidationException(strNo, pos, expected, found) =>
      val f = if(found == '\u0000') "end of input" else s"'$found'"
      Some((strNo, pos,
          s"Failed to parse JsonBuffer literal: Expected $expected, but found $f."))
  }
  
  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[JsonBuffer]]*)
      (parser: c.Expr[Parser[String, JsonBufferAst]]): c.Expr[JsonBuffer] =
    super.contextMacro(c)(exprs: _*)(parser)
}

/** Provides support for JSON literals, in the form json" { } " or json""" { } """.
  * Interpolation is used to substitute variable names into the JSON, and to extract values
  * from a JSON string. */
private[json] class JsonStrings(sc: StringContext) {
  class JsonContext() extends DataContext(Json, sc) {
    def apply(exprs: ForcedConversion[Json]*)(implicit parser: Parser[String, JsonAst]): Json =
      macro JsonDataMacros.contextMacro
  }
  val json = new JsonContext()
}

private[json] class JsonBufferStrings(sc: StringContext) {
  class JsonBufferContext() extends DataContext(JsonBuffer, sc) {
    def apply(exprs: ForcedConversion[JsonBuffer]*)(implicit parser: Parser[String,
        JsonBufferAst]): JsonBuffer =
      macro JsonBufferDataMacros.contextMacro
  }
  val jsonBuffer = new JsonBufferContext()
}
