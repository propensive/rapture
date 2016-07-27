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

package rapture.base

import scala.reflect._
import macros._

object `package` {
  type BlackboxContext = blackbox.Context
  type WhiteboxContext = whitebox.Context

  lazy val compatibility = new Compat211()
}

class Compat211() {
  def termName[C <: blackbox.Context](c: C, s: String) = c.universe.TermName(s)
  def typeName[C <: blackbox.Context](c: C, s: String) = c.universe.TypeName(s)
  def constructor[C <: blackbox.Context](c: C) = c.universe.termNames.CONSTRUCTOR
  def wildcard[C <: blackbox.Context](c: C) = c.universe.termNames.WILDCARD
  def typeIntersection[C <: blackbox.Context](c: C)(xs: List[c.universe.Type]) =
    c.universe.internal.intersectionType(xs)
  def paramLists[C <: blackbox.Context](c: C)(t: c.universe.MethodSymbol) = t.paramLists
  def normalize[C <: blackbox.Context](c: C)(t: c.universe.Type) = t.dealias
  def declarations[C <: blackbox.Context](c: C)(t: c.universe.Type) = t.decls
  def declaration[C <: blackbox.Context](c: C)(t: c.universe.Type, d: c.universe.Name) = t.decl(d)
  def readLine(): String = scala.io.StdIn.readLine
  def typecheck[C <: blackbox.Context](c: C)(t: c.Tree) = c.typecheck(t)
  def freshName[C <: blackbox.Context](c: C)(s: String) = c.freshName(s)
  def companion[C <: blackbox.Context](c: C)(s: c.universe.Symbol) = s.companion
}
