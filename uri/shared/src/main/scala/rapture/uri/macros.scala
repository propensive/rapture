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

package rapture.uri

import rapture.base._
import rapture.core._

object Paramable {
  implicit val stringParamable = new Paramable[String] { def paramize(s: String): String = s }
  implicit val intParamable = new Paramable[Int] { def paramize(s: Int): String = s.toString }
  implicit val doubleParamable = new Paramable[Double] { def paramize(s: Double): String = s.toString }
}

trait Paramable[T] {
  def paramize(t: T): String
}

object UriMacros {

  def uriMacro(c: WhiteboxContext)(content: c.Expr[String]*): c.Expr[Any] = {
    import c.universe._
    import compatibility._

    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        rawParts.head match {
          case Literal(Constant(part: String)) =>
            val scheme = part.split(":", 2) match {
              case Array(s, _) => s
              case _ => c.abort(c.enclosingPosition, "Could not find a valid scheme for this URI.")
            }

            val constants = rawParts match {
              case Literal(Constant(h: String)) :: t =>
                q"${h.substring(scheme.length + 1)}" :: t
            }

            val variables = content.map(_.tree).to[List]
            c.Expr(q"_root_.rapture.uri.UriContext.${termName(c, scheme)}(List(..$constants))(List(..$variables))")
        }
    }
  }
}
