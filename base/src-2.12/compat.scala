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
  def typeIntersection[C <: blackbox.Context](c: C)(xs: List[c.universe.Type]) = c.universe.internal.intersectionType(xs)
  def paramLists[C <: blackbox.Context](c: C)(t: c.universe.MethodSymbol) = t.paramLists
  def normalize[C <: blackbox.Context](c: C)(t: c.universe.Type) = t.dealias
  def declarations[C <: blackbox.Context](c: C)(t: c.universe.Type) = t.decls
  def readLine(): String = scala.io.StdIn.readLine
  def typecheck[C <: blackbox.Context](c: C)(t: c.Tree) = c.typecheck(t)
  def freshName[C <: blackbox.Context](c: C)(s: String) = c.freshName(s)
}
