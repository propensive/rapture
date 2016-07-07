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

import rapture.core._

import language.experimental.macros
import scala.language.implicitConversions

object UriContext

object `package` {

  implicit class EnrichedStringContext(sc: StringContext) {
    def uri(content: String*): Any = macro UriMacros.uriMacro
  }

  implicit class EnrichedUriContext(uc: UriContext.type) {
    def classpath(constants: List[String])(variables: List[String]) =
      new ClasspathUrl(constants.zip(variables :+ "").map { case (a, b) => a + b }.mkString.split("/").to[Vector])
  }

  val $ : String = ""

  val !! : String = ".."

  object ^ extends RootedPath(Vector())

  implicit def dereferenceable[Res](res: Res): Dereferenceable.Capability[Res] = alloc(res)
  implicit def uriCapable[Res: UriCapable](res: Res): UriCapable.Capability[Res] = alloc(res)
  implicit def linkCapable[Res: Linkable](res: Res): Linkable.Capability[Res] = alloc(res)
  implicit def parentable[Res](res: Res): Parentable.Capability[Res] = alloc(res)
  implicit def navigableExtras[Res: Navigable](url: Res): NavigableExtras[Res] =
    new NavigableExtras(url)

}
