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

package rapture.core

import rapture.base._
import scala.reflect._

private[core] object CoreMacros {
  def enumerateMacro[Cls: c.WeakTypeTag, T: c.WeakTypeTag](c: BlackboxContext)(value: c.Expr[Cls]): c.Expr[List[T]] = {

    import c.universe._
    import compatibility._

    val cls = weakTypeOf[Cls]
    val allMethods = weakTypeOf[Cls].members.to[List].filter(_.isMethod).map(_.asMethod)
    val matchingMethods = allMethods filter { m =>
      paramLists(c)(m).isEmpty && m.returnType.weak_<:<(weakTypeOf[T])
    }
    val methodNames = matchingMethods map { m =>
      Select(value.tree, termName(c, m.name.toString))
    }
    val listApply = Select(reify(List).tree, termName(c, "apply"))

    c.Expr[List[T]](Apply(listApply, methodNames))
  }

  def assignedMethodNameMacro(c: BlackboxContext): c.Expr[MethodName] = {
    import c.universe._
    import compatibility._

    val name = enclosingDef(c)(c.macroApplication.pos).map { name =>
      c.Expr[MethodName](q"new _root_.rapture.core.MethodName(${name.decodedName.toString.trim})")
    }
    
    name getOrElse c.abort(c.enclosingPosition, "this method invocation must be assigned to a named identifier.")
  }

  object AssignedNameMacroState {
    var lastPoint: Option[api.Position] = None
    var assignmentCount: Int = 0
  }

  def assignedNameMacro(c: BlackboxContext): c.Expr[AssignedName] = {
    import c.universe._
    import AssignedNameMacroState._
    import compatibility._

    val currentPoint = c.macroApplication.pos

    if(Some(currentPoint) != lastPoint) assignmentCount = 0

    val name = enclosingVals(c)(currentPoint, assignmentCount).map { name =>
      c.Expr[AssignedName](q"_root_.rapture.core.AssignedName(${name.decodedName.toString.trim})")
    }

    lastPoint = Some(currentPoint)
    assignmentCount += 1

    name getOrElse c.abort(c.enclosingPosition, "this method invocation must be assigned to a named identifier.")
  }

  def allocMacro[T: c.WeakTypeTag](c: BlackboxContext): c.Expr[Alloc0[T]] = {
    import c.universe._
    import compatibility._

    val construction = c.Expr[T](Apply(Select(New(TypeTree(weakTypeOf[T])), constructor(c)), List()))
    reify { new Alloc0[T] { def instantiate(): T = construction.splice } }
  }

  def allocMacro1[T: c.WeakTypeTag, P1: c.WeakTypeTag](c: BlackboxContext): c.Expr[Alloc1[T, P1]] = {
    import c.universe._
    import compatibility._

    val construction =
      c.Expr[T](Apply(Select(New(TypeTree(weakTypeOf[T])), constructor(c)), List(Ident(termName(c, "p1")))))

    reify { new Alloc1[T, P1] { def instantiate(p1: P1): T = construction.splice } }
  }

  def allocMacro2[T: c.WeakTypeTag, P1: c.WeakTypeTag, P2: c.WeakTypeTag](
      c: BlackboxContext): c.Expr[Alloc2[T, P1, P2]] = {

    import c.universe._
    import compatibility._

    val construction = c.Expr[T](
        Apply(Select(New(TypeTree(weakTypeOf[T])), constructor(c)),
              List(Ident(termName(c, "p1")), Ident(termName(c, "p2")))))
    reify { new Alloc2[T, P1, P2] { def instantiate(p1: P1, p2: P2): T = construction.splice } }
  }

  def allocMacro3[T: c.WeakTypeTag, P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag](
      c: WhiteboxContext): c.Expr[Alloc3[T, P1, P2, P3]] = {

    import c.universe._
    import compatibility._

    val construction = c.Expr[T](
        Apply(Select(New(TypeTree(weakTypeOf[T])), constructor(c)),
              List(Ident(termName(c, "p1")), Ident(termName(c, "p2")), Ident(termName(c, "p3")))))

    reify { new Alloc3[T, P1, P2, P3] { def instantiate(p1: P1, p2: P2, p3: P3): T = construction.splice } }
  }

  def allocMacro4[T: c.WeakTypeTag, P1: c.WeakTypeTag, P2: c.WeakTypeTag, P3: c.WeakTypeTag, P4: c.WeakTypeTag](
      c: WhiteboxContext): c.Expr[Alloc4[T, P1, P2, P3, P4]] = {

    import c.universe._
    import compatibility._

    val construction = c.Expr[T](
        Apply(Select(New(TypeTree(weakTypeOf[T])), constructor(c)),
              List(Ident(termName(c, "p1")),
                   Ident(termName(c, "p2")),
                   Ident(termName(c, "p3")),
                   Ident(termName(c, "p4")))))

    reify {
      new Alloc4[T, P1, P2, P3, P4] { def instantiate(p1: P1, p2: P2, p3: P3, p4: P4): T = construction.splice }
    }
  }
}
