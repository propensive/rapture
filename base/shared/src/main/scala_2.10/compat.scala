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
  type BlackboxContext = Context
  type WhiteboxContext = Context

  lazy val compatibility = new Compat210()
}

class Compat210() {
  def termName(c: Context, s: String) = c.universe.newTermName(s)
  def typeName(c: Context, s: String) = c.universe.newTypeName(s)
  def constructor(c: Context) = c.universe.nme.CONSTRUCTOR
  def wildcard(c: Context) = c.universe.nme.WILDCARD
  def typeIntersection(c: Context)(xs: List[c.universe.Type]) = c.universe.intersectionType(xs)
  def paramLists(c: Context)(t: c.universe.MethodSymbol) = t.paramss
  def normalize(c: Context)(t: c.universe.Type) = t.normalize
  def declarations(c: Context)(t: c.universe.Type) = t.declarations
  def declaration(c: Context)(t: c.universe.Type, n: c.universe.Name) = t.declaration(n)
  def readLine(): String = Console.readLine()
  def typecheck(c: Context)(s: c.Tree) = c.typeCheck(s)
  def freshName(c: Context)(s: String) = c.fresh(s)
  def companion(c: Context)(x: c.universe.Symbol) = x.companionSymbol
}
