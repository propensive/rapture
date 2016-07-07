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

package rapture.js

import rapture.core._

import javax.script._

private[js] object JsValidator {

  case class ValidationException(strNo: Int, pos: Int, msg: String) extends Exception

  def validate(parts: List[String]): Unit = {
    val script = parts.mkString("null")
    val engine: Compilable = alloc[ScriptEngineManager]().getEngineByName("JavaScript") match {
      case e: Compilable => e
    }

    try engine.compile(script)
    catch {
      case e: ScriptException =>
        val pos = script.split("\n").take(e.getLineNumber - 1).map(_.length + 1).sum + e.getColumnNumber
        val Regex = "<eval>:[0-9]+:[0-9]+ (.*)$".r
        println("pos = " + pos)
        val msg = e.getMessage.split("\n").head match { case Regex(m) => m }

        throw ValidationException(0, pos, msg)
    }
  }
}
