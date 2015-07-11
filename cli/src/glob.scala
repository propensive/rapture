/******************************************************************************************************************\
* Rapture CLI, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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
package rapture.cli

import java.util.regex._

object globInterpreters {
  /* This in an incomplete implementation as it does not support character classes enclosed
   * by `[` and `]`.
   */
  object unix {
    def apply(): GlobInterpreter = implicitGlobInterpreter
    implicit val implicitGlobInterpreter: GlobInterpreter = new GlobInterpreter {
      def interpret(glob: String): Pattern = {
        val sb = new StringBuilder
        var start = true
        glob foreach { c =>
          start = false
          sb.append(c match {
            case '*' => if(start) "[^./][^/]*" else "[^/]*"
            case '?' => if(start) "[^./][^/]*" else "[^/]*"
            case '/' => start = true; "/"
            case esc@('.' | '[' | '{' | '(' | '+' | '^' | '$' | '|') => "\\"+esc
            case other => other.toString
          })
        }
        Pattern.compile(sb.toString)
      }
    }
  }
}

trait GlobInterpreter { def interpret(glob: String): Pattern }

case class Glob(globString: String)(implicit globInterpreter: GlobInterpreter) {
  lazy val pattern: Pattern = globInterpreter.interpret(globString)
  def matches(s: String) = pattern.matcher(s).matches
}
