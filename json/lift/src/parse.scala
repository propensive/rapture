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
package rapture.json.jsonBackends.lift

import rapture.core._
import rapture.json._
import rapture.data._

import scala.collection.mutable.{ListBuffer, HashMap}
import scala.collection.JavaConverters
import net.liftweb.json._

private[lift] object LiftParser extends Parser[String, JsonBufferAst] {
  val ast = LiftAst
  def parse(s: String): Option[Any] =
    try Some(JsonParser.parse(s)) catch { case e: Exception => None }

  override def toString = "<LiftParser>"
}
