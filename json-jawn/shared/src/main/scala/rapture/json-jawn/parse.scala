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

package rapture.json.jsonBackends.jawn

import rapture.core._
import rapture.data._
import rapture.json._

import jawn.{Parser => JawnParser, _}

private[jawn] class JawnStringParser(implicit f: Facade[_]) extends Parser[String, JsonBufferAst] {

  override def toString = "<JawnStringParser>"

  val ast = JawnAst
  def parse(s: String): Option[Any] = JawnParser.parseFromString(s).toOption
}

private[jawn] class JawnByteBufferParser(implicit f: Facade[_]) extends Parser[java.nio.ByteBuffer, JsonBufferAst] {

  override def toString = "<JawnByteBufferParser>"

  val ast = JawnAst
  def parse(buf: java.nio.ByteBuffer): Option[Any] =
    JawnParser.parseFromByteBuffer(buf).toOption
}

private[jawn] class JawnFileParser(implicit f: Facade[_]) extends Parser[java.io.File, JsonBufferAst] {

  override def toString = "<JawnFileParser>"

  val ast = JawnAst
  def parse(file: java.io.File): Option[Any] = JawnParser.parseFromFile(file).toOption
}
