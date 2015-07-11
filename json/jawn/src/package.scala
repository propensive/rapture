/**********************************************************************************************\
* Rapture JSON Library                                                                         *
* Version 1.1.0                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2014 Jon Pretty, Propensive Ltd.                                              *
*                                                                                              *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file    *
* except in compliance with the License. You may obtain a copy of the License at               *
*                                                                                              *
*   http://www.apache.org/licenses/LICENSE-2.0                                                 *
*                                                                                              *
* Unless required by applicable law or agreed to in writing, software distributed under the    *
* License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    *
* either express or implied. See the License for the specific language governing permissions   *
* and limitations under the License.                                                           *
\**********************************************************************************************/
package rapture.json.jsonBackends.jawn

import rapture.core._
import rapture.data._
import rapture.json._

import _root_.jawn.{Parser => _, _}
import _root_.jawn.ast._

private[jawn] trait package_1 {
  implicit val jawnFacade: Facade[JValue] = JawnFacade
}

object `package` extends package_1 with Extractors with Serializers {
  implicit val implicitJsonAst: JsonBufferAst = JawnAst
  implicit def implicitJsonStringParser(implicit f: Facade[_]): Parser[String, JsonBufferAst] =
    new JawnStringParser
  
  implicit def implicitJsonByteBufferParser(implicit f: Facade[_]): Parser[java.nio.ByteBuffer,
      JsonBufferAst] = new JawnByteBufferParser
  
  implicit def implicitJsonFileParser(implicit f: Facade[_]): Parser[java.io.File,
      JsonBufferAst] = new JawnFileParser

  implicit def jsonFormatterImplicit[Ast <: JsonAst](implicit ast: Ast): Formatter[Ast] {
      type Out = String } = new Formatter[Ast] {
    type Out = String
    def format(json: Any): String = json match {
      case jv: JValue => jv.render(FastRenderer)
      case _ => ???
    }
  }
}
