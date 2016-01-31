/******************************************************************************************************************\
* Rapture Test, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
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
package rapture.test

import scala.language.experimental.macros
import java.util.regex.Pattern
import rapture.base._

object deferTypeErrors {
  implicit def deferTypeErrorsConvertAnyToAny[T, S](t: T): S = ???
  implicit def deferTypeErrorsResolveAnyImplicit[T]: T = ???
}

object typeMismatch {

  def apply[T](fn: => T): Boolean = macro applyMacro[T]

  def applyMacro[T](c: BlackboxContext)(fn: c.Expr[T]): c.Expr[Boolean] = {
    import c.universe._

    val found = fn.tree.exists {
      case Select(_, name) => name.decodedName.toString match {
        case "deferTypeErrorsConvertAnyToAny" => true
        case "deferTypeErrorsResolveAnyImplicit" => true
        case _ => false
      }
      case _ => false
    }

    c.Expr[Boolean](Literal(Constant(found)))
  }
}
