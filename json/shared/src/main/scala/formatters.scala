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

import scala.reflect.macros._
import scala.annotation._

import language.experimental.macros
import language.higherKinds

object formatters extends formatters_1 {
  object compact {
    def apply[Ast <: JsonAst]()(implicit ast: Ast): Formatter[Ast] { type Out = String } =
      jsonFormatterImplicit[Ast]

    implicit def jsonFormatterImplicit[Ast <: JsonAst](implicit ast: Ast):
        Formatter[Ast] { type Out = String } =
      new Formatter[Ast] {
        type Out = String
        def format(json: Any): String = general(json, 0, ast, "", "")  
      }
  }
}

private[json] class formatters_1 {
  /** Formats the JSON object for multi-line readability. */
  protected def general[Ast <: JsonAst](json: Any, ln: Int, ast: Ast, pad: String = " ",
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
          val bd = ast.getBigDecimal(j)
          if(bd.isWhole) String(bd.toBigInt) else String(bd)
        } else if(ast.isArray(j)) {
          val arr = ast.getArray(j)
          if(arr.isEmpty) "[]" else List("[", arr map { v =>
            s"${indent}${pad}${general(v, ln + 1, ast, pad, brk)}"
          } mkString s",${brk}", s"${indent}]") mkString brk
        } else if(ast.isObject(j)) {
          val keys = ast.getKeys(j)
          if(keys.isEmpty) "{}" else List("{", keys map { k =>
            val inner = ast.dereferenceObject(j, k)
            s"""${indent}${pad}"${k}":${pad}${general(inner, ln + 1, ast, pad, brk)}"""
          } mkString s",${brk}", s"${indent}}") mkString brk
        } else if(ast.isNull(j)) "null"
        else if(j == DataCompanion.Empty) "empty"
        else "undefined"
    }
  }
 
  object humanReadable {
    def apply[Ast <: JsonAst]()(implicit ast: Ast): Formatter[Ast] { type Out = String } =
      jsonFormatterImplicit[Ast]

    implicit def jsonFormatterImplicit[Ast <: JsonAst](implicit ast: Ast):
        Formatter[Ast] { type Out = String } =
      new Formatter[Ast] {
        type Out = String
        def format(json: Any): String = general(json, 0, ast, " ", "\n")  
      }
  }

}
