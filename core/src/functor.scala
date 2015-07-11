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

import scala.reflect.runtime.universe.TypeTag
import language.higherKinds

trait Functor[+F[x] <: Functor[F, x], A] { functor =>
  type Throws <: Exception

  protected def rawMap[B](fn: (A, Mode[_ <: MethodConstraint]) => B): F[B]
  
  def map[B](fn: A => B): F[B] { type Throws = functor.Throws with Exception } =
    emap[Exception](fn)

  def emap[E <: Exception]: Emap[E] = new Emap[E]()
  
  def smap[B](fn: A => B): F[B] { type Throws = functor.Throws } =
    emap[Nothing](fn).asInstanceOf[F[B] { type Throws = functor.Throws }]
  
  // FIXME: Make this a value class
  class Emap[E <: Exception]() {
    def apply[B](fn: A => B)(implicit tt: TypeTag[E]): F[B] { type Throws = functor.Throws with E } =
      functor.rawMap { case (a, m) =>
        try fn(a) catch { case e: Exception => m.exception[B, E](e.asInstanceOf[E]) }
      }.asInstanceOf[F[B] { type Throws = functor.Throws with E }]
  }
}

