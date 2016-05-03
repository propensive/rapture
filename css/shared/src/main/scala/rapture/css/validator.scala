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

package rapture.css

import rapture.core._

import com.steadystate.css.parser._
import org.w3c.css.sac._
import java.io._

private[css] object CssValidator {

  case class ValidationException(strNo: Int, pos: Int, msg: String)
      extends Exception

  def validate(parts: List[String], stylesheet: Boolean): Unit = {
    val errHandler = new ErrorHandler {
      def error(e: CSSParseException) =
        throw ValidationException(0, e.getColumnNumber - 1, e.getMessage)
      def fatalError(e: CSSParseException) = error(e)
      def warning(e: CSSParseException) = error(e)
    }
    val source = new InputSource(new StringReader(parts.mkString("null")))
    val parser = new CSSOMParser(new SACParserCSS3())
    parser.setErrorHandler(errHandler)

    if (stylesheet) {
      parser.parseStyleSheet(source, null, null)
    } else {
      val ss = parser.parseStyleDeclaration(source)
      for (i <- 0 until ss.getLength) if (!Properties.all.contains(ss.item(i)))
        throw ValidationException(
            0, 0, s"invalid CSS attribute '${ss.item(i)}'")
    }
  }
}
