/******************************************************************************************************************\
* Rapture Core, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
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
package rapture.core

import language.experimental.macros

object AssignedName {
  implicit def assignedNameImplicit: AssignedName = macro CoreMacros.assignedNameMacro
}
class AssignedName(val name: String) extends AnyVal

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
