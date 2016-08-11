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

import language.experimental.macros
import language.higherKinds

object AssignedName {
  implicit def assignedNameImplicit: AssignedName = macro CoreMacros.assignedNameMacro
}
case class AssignedName(name: String) extends AnyVal {
  override def toString = s"`$name`"
}

object MethodName {
  implicit def assignedMethodNameImplicit: MethodName = macro CoreMacros.assignedMethodNameMacro
}
class MethodName(val name: String) extends AnyVal

trait Cell[T] {
  def apply(): T
  def update(t: T): Unit
}

object Cell {
  def apply[T](get: => T)(set: T => Unit): Cell[T] = new Cell[T] {
    def apply() = get
    def update(t: T) = set(t)
  }
}

object Var {
  def apply[T](t: T) = new Cell[T] {
    private var value = t
    def apply(): T = value
    def update(t: T) = value = t
  }
}


object OptionalParameter {
  implicit def autoWrapSpecifiedParameter[T](value: T): OptionalParameter[T] = SpecifiedParameter[T](value)
}

sealed trait OptionalParameter[+T] { def apply(): Option[T] }

case class SpecifiedParameter[+T] (value: T) extends OptionalParameter[T] {
  def apply(): Option[T] = Some(value)
}

case object UnspecifiedParameter extends OptionalParameter[Nothing] {
  def apply(): Option[Nothing] = None
}


object Annex {
  implicit def annexValueWithTypeclass[V, Tc[_]](v: V)(implicit tc: Tc[V]): Annex[Tc] =
    new Annex[Tc] {
      type Value = V
      def value: Value = v
      def typeclass: Tc[Value] = tc
    }
}

abstract class Annex[Typeclass[_]] {
  type Value
  def value: Value
  def typeclass: Typeclass[Value]
  def apply[Return](fn: Typeclass[Value] => Value => Return): Return = fn(typeclass)(value)
}


