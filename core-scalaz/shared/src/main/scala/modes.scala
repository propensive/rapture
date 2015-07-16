/**********************************************************************************************\
* Rapture Core/Scalaz Library                                                                  *
* Version 1.0.0                                                                                *
*                                                                                              *
* The primary distribution site is                                                             *
*                                                                                              *
*   http://rapture.io/                                                                         *
*                                                                                              *
* Copyright 2010-2014 Jon Pretty, Propensive Ltd.                                              *
*                                                                                              *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file    *
* except in compliance with the License. You may obtain a copy of the License at               *
*                                                                                              *
*   http://www.apache.org/licenses/LICENSE-2.0                                                 *
*                                                                                              *
* Unless required by applicable law or agreed to in writing, software distributed under the    *
* License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    *
* either express or implied. See the License for the specific language governing permissions   *
* and limitations under the License.                                                           *
\**********************************************************************************************/
package rapture.core.scalazModes

import language.higherKinds

import rapture.core._

import java.util.concurrent.ExecutorService

import scalaz._
import scalaz.concurrent._
import scala.reflect._

object `package` {

  implicit class ScalazExplicits[+T, E <: Exception](explicit: modes.Explicit[T, E]) {
    def task(implicit pool: ExecutorService): Task[T] =
      returnTasks.wrap(explicit.get)

    def valid: Validation[E, T] = returnValidations.wrap(explicit.get)
  }

  implicit def returnTasks[Group <: ModeGroup](implicit pool: ExecutorService) = new ReturnTasks[Group]
  class ReturnTasks[+Group <: ModeGroup](implicit pool: ExecutorService) extends Mode[Group] {
    type Wrap[+T, E <: Exception] = Task[T]
    def wrap[T, E <: Exception: ClassTag](t: => T): Task[T] = Task.delay(t)
  }

  implicit def returnValidations[Group <: ModeGroup] = new ReturnValidation[Group]
  class ReturnValidation[+Group <: ModeGroup] extends Mode[Group] {
    type Wrap[+T, E <: Exception] = Validation[E, T]
    def wrap[T, E <: Exception: ClassTag](t: => T): Validation[E, T] =
      try Success(t) catch { case e: E => Failure(e) }
  }

  implicit def returnDisjunction[Group <: ModeGroup] = new ReturnDisjunction[Group]
  class ReturnDisjunction[+Group <: ModeGroup] extends Mode[Group] {
    type Wrap[+T, E <: Exception] = \/[E, T]
    def wrap[T, E <: Exception: ClassTag](t: => T): \/[E, T] =
      try \/-(t) catch { case e: E => -\/(e) }
  }

}
