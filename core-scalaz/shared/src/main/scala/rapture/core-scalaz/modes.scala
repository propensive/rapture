/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.core.scalazInterop

import language.higherKinds

import rapture.core._

import java.util.concurrent.ExecutorService

import scalaz._
import scalaz.concurrent._

object `package` {

  implicit class ScalazExplicits[+T, E <: Exception](explicit: modes.Explicitly[T, E]) {
    def task(implicit pool: ExecutorService): Task[T] = returnTasks.wrap(explicit.get)
    def validation: Validation[E, T] = returnValidations.wrap(explicit.get)
  }

  implicit def returnTasks[Group <: MethodConstraint](implicit pool: ExecutorService) = new ReturnTasks[Group]
  class ReturnTasks[+Group <: MethodConstraint](implicit pool: ExecutorService) extends Mode[Group] {
    type Wrap[+T, E <: Exception] = Task[T]
    def wrap[T, E <: Exception](t: => T): Task[T] = Task.delay(t)
    def unwrap[T](t: => Wrap[T, _ <: Exception]): T = t.attemptRun.valueOr { throw _ }
  }

  // FIXME: This should be modified to collect multiple failures
  implicit def returnValidations[Group <: MethodConstraint] = new ReturnValidation[Group]
  class ReturnValidation[+Group <: MethodConstraint] extends Mode[Group] {
    type Wrap[+T, E <: Exception] = Validation[E, T]
    def wrap[T, E <: Exception](t: => T): Validation[E, T] = try Success(t) catch { case e: E => Failure(e) }
    def unwrap[T](t: => Validation[_ <: Exception, T]): T = t.valueOr { throw _ }
  }

  implicit def returnDisjunction[Group <: MethodConstraint] = new ReturnDisjunction[Group]
  class ReturnDisjunction[+Group <: MethodConstraint] extends Mode[Group] {
    type Wrap[+T, E <: Exception] = \/[E, T]
    def wrap[T, E <: Exception](t: => T): \/[E, T] = try \/-(t) catch { case e: E => -\/(e) }
    def unwrap[T](t: => \/[_ <: Exception, T]): T = t.valueOr { throw _ }
  }

}
