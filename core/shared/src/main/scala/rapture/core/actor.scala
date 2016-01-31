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

import scala.concurrent._
import scala.util._

sealed trait ActorResponse[+T, +S]

case class Reply[+T](reply: T) extends ActorResponse[T, Nothing] {
  def andUpdate[S](state: S) = Update[T, S](reply, state)
}
case class Update[+T, +S](reply: T, state: S) extends ActorResponse[T, S]
case object Ignore extends ActorResponse[Nothing, Nothing]

object Actor {
  class ActorOf[Msg] {
    def apply[Res, State](init: State)(fn: Transition[Msg, State] => ActorResponse[Res, State])(implicit ec: ExecutionContext): Actor[Msg, Res, State] =
      new Actor[Msg, Res, State](init) {
        def handle(trans: Transition[Msg, State]): ActorResponse[Res, State] = fn(trans)
      }
  }

  def of[Msg] = new ActorOf[Msg]
}

case class IgnoredException() extends Exception("Message was ignored")

abstract class Actor[Msg, Res, State](init: State)(implicit executionContext: ExecutionContext) {
 
  private var future: Future[Res] = Future.successful(null.asInstanceOf[Res])
  private var stateVar: State = init

  protected def enqueue(fn: => ActorResponse[Res, State]): Future[Res] = future.synchronized {
    val promise = Promise[Res]
    future = future.andThen { case _ =>
      val result = Try(fn) match {
        case Success(Ignore) =>
          promise.failure(IgnoredException())
        case Success(Update(r, s)) =>
          promise.success(r)
          stateVar = s
        case Success(Reply(r)) =>
          promise.success(r)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  def state: State = stateVar

  def cue(msg: Msg): Future[Res] = enqueue { handle(Transition(msg, stateVar)) }

  def handle(trans: Transition[Msg, State]): ActorResponse[Res, State]
}

case class Transition[Msg, State](msg: Msg, state: State)
