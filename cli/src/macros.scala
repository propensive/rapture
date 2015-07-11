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

import scala.reflect.macros._

private[cli] object CliMacros {


  // Copied from URI macro as a starting point
  /*def shImplementation(c: Context)(content: c.Expr[String]*): c.Expr[Process] = {
    import c.universe._

    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        val xs = content.map(_.tree).to[Array]
        val ys = rawParts.to[Array]

        rawParts.head match {
          case Literal(Constant(part: String)) =>
            val Array(scheme, _) = part.split(":", 2)
            c.Expr(
              Apply(
                Select(
                  Ident(
                    newTermName(scheme.capitalize)
                  ),
                  newTermName("parse")
                ),
                List(
                  Apply(
                    Select(
                      Apply(
                        Select(
                          Ident(newTermName("List")),
                          newTermName("apply")
                        ),
                        (0 until (xs.length + ys.length) map { i => if(i%2 == 0) ys(i/2) else xs(i/2) }).to[List]
                      ),
                      newTermName("mkString")
                    ),
                    List(Literal(Constant("")))
                  )
                )
              )
            )
        }
    }
  }*/
}
