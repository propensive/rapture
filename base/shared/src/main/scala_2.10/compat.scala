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
  def readLine(): String = Console.readLine()
  def typecheck(c: Context)(s: c.Tree) = c.typeCheck(s)
  def freshName(c: Context)(s: String) = c.fresh(s)
}
