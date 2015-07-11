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
import scala.reflect._
import scala.reflect.macros._

import annotation.unchecked._

class AllocApply[T](val unit: Int) extends AnyVal {
  def apply()(implicit inst: Alloc0.Invariant[T]): T = inst.instantiate()
  def apply[P1](p1: P1)(implicit inst: Alloc1.Invariant[T, P1]): T = inst.instantiate(p1)
  def apply[P1, P2](p1: P1, p2: P2)(implicit inst: Alloc2.Invariant[T, P1, P2]): T = inst.instantiate(p1, p2)
  def apply[P1, P2, P3](p1: P1, p2: P2, p3: P3)(implicit inst: Alloc3.Invariant[T, P1, P2, P3]): T = inst.instantiate(p1, p2, p3)
  def apply[P1, P2, P3, P4](p1: P1, p2: P2, p3: P3, p4: P4)(implicit inst: Alloc4.Invariant[T, P1, P2, P3, P4]): T = inst.instantiate(p1, p2, p3, p4)
}

object Alloc0 {
  implicit def alloc0[T]: Alloc0[T] = macro CoreMacros.allocMacro[T]
  type Invariant[+T] = Alloc0[T @uncheckedVariance]
}

object Alloc1 {
  implicit def alloc1[T, P1]: Alloc1[T, P1] = macro CoreMacros.allocMacro1[T, P1]
  type Invariant[+T, P1] = Alloc1[T @uncheckedVariance, P1]
}

object Alloc2 {
  implicit def alloc2[T, P1, P2]: Alloc2[T, P1, P2] = macro CoreMacros.allocMacro2[T, P1, P2]
  type Invariant[+T, P1, P2] = Alloc2[T @uncheckedVariance, P1, P2]
}

object Alloc3 {
  implicit def alloc3[T, P1, P2, P3]: Alloc3[T, P1, P2, P3] = macro CoreMacros.allocMacro3[T, P1, P2, P3]
  type Invariant[+T, P1, P2, P3] = Alloc3[T @uncheckedVariance, P1, P2, P3]
}

object Alloc4 {
  implicit def alloc4[T, P1, P2, P3, P4]: Alloc4[T, P1, P2, P3, P4] = macro CoreMacros.allocMacro4[T, P1, P2, P3, P4]
  type Invariant[+T, P1, P2, P3, P4] = Alloc4[T @uncheckedVariance, P1, P2, P3, P4]
}

@implicitNotFound("No constructor exists for instantiating an object of this type")
trait Alloc0[T] { def instantiate(): T }

@implicitNotFound("No constructor exists for instantiating an object of this type")
trait Alloc1[T, P1] { def instantiate(p1: P1): T }

@implicitNotFound("No constructor exists for instantiating an object of this type")
trait Alloc2[T, P1, P2] { def instantiate(p1: P1, p2: P2): T }

@implicitNotFound("No constructor exists for instantiating an object of this type")
trait Alloc3[T, P1, P2, P3] { def instantiate(p1: P1, p2: P2, p3: P3): T }

@implicitNotFound("No constructor exists for instantiating an object of this type")
trait Alloc4[T, P1, P2, P3, P4] { def instantiate(p1: P1, p2: P2, p3: P3, p4: P4): T }
