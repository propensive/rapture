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

import language.{existentials, higherKinds}
import scala.reflect._
import scala.util._
import scala.concurrent._

trait MethodConstraint

object Mode extends Mode_1 {
  abstract class Import[M[G <: MethodConstraint] <: Mode[G]] {
    def apply[G <: MethodConstraint](): M[G] = modeImplicit[G]
    implicit def modeImplicit[G <: MethodConstraint]: M[G] = mode[G]
    protected def mode[G <: MethodConstraint]: M[G]
  }
}

@implicitNotFound(
    msg = "No implicit mode was available for $"+"{Group} methods. " +
        "Please import a member of rapture.core.modes, e.g. modes.throwExceptions.")
trait Mode[+Group <: MethodConstraint] { mode =>
  type Wrap[+_, _ <: Exception]
  def wrap[Res, E <: Exception](blk: => Res): Wrap[Res, E]

  def flatWrap[Res, E <: Exception: ClassTag](blk: => Wrap[Res, E]): Wrap[Res, E] =
    wrap(unwrap(blk))

  var callPath = "_"

  def unwrap[Res](value: => Wrap[Res, _ <: Exception]): Res
  def unwrap[Res](value: => Wrap[Res, _ <: Exception], path: String): Res = {
    val oldCallPath = callPath
    callPath += path
    val res = unwrap[Res](value)
    callPath = oldCallPath
    res
  }

  def generic[C <: MethodConstraint]: Mode[C] { type Wrap[+T, E <: Exception] = mode.Wrap[T, E] } =
    this.asInstanceOf[Mode[C] { type Wrap[+T, E <: Exception] = mode.Wrap[T, E] }]

  def compose[Group2 <: MethodConstraint](mode2: Mode[Group2]) = new Mode[Group] {
    type Wrap[+Res, E <: Exception] = mode.Wrap[mode2.Wrap[Res, E], E]

    def wrap[Res, E <: Exception](blk: => Res): Wrap[Res, E] =
      mode.wrap(mode2.wrap(blk))

    def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return =
      mode2.unwrap(mode.unwrap(value))
  }

  def catching[E <: Exception: ClassTag, T](blk: => T) =
    try blk
    catch {
      case e: E => exception(e)
      case e: Exception => throw e
    }

  def safe[T](blk: => T): T = {
    try blk
    catch { case e: Exception => exception(e) }
  }
  def exception[T, E <: Exception: ClassTag](e: E, continue: Boolean = true): T = throw e

  def wrapEither[Res, E <: Exception: ClassTag](blk: => Either[E, Res]): Wrap[Res, E] =
    wrap {
      blk match {
        case Left(e) => throw e
        case Right(r) => r
      }
    }

  def wrapOption[Res](blk: => Option[Res]): Wrap[Res, Exception] = wrap(blk.get)

  def wrapTry[Res, E <: Exception: ClassTag](blk: => Try[Res]): Wrap[Res, E] =
    wrap(blk.get)
}

object repl {

  var showStackTraces: Boolean = false
  private var lastExceptionValue: Throwable = new SilentException

  def lastException: Nothing = throw lastExceptionValue

  implicit def modeImplicit[Group <: MethodConstraint] = new Repl[Group]

  class SilentException extends Throwable {
    override def printStackTrace(pw: java.io.PrintWriter) = ()
  }

  class Repl[+Group <: MethodConstraint] extends Mode[Group] {

    type Wrap[+Return, E <: Exception] = T2 forSome { type T2 <: Return }
    def wrap[Return, E <: Exception](blk: => Return): Return =
      try blk
      catch {
        case e: Exception =>
          if (showStackTraces) throw e
          else {
            Console.println("Execution failed with exception: " + e.toString)
            Console.print("For the full stacktrace, see repl.lastException.")
            lastExceptionValue = e
            throw new SilentException()
          }
      }

    def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value
  }
}

package modes {

  object throwExceptions extends Mode.Import[ThrowExceptionsMode] {
    protected def mode[G <: MethodConstraint] = new ThrowExceptionsMode[G]
  }

  object explicit extends Mode.Import[ExplicitMode] {
    protected def mode[G <: MethodConstraint] = new ExplicitMode[G]
  }

  /*object returnEither extends Mode.Import[ReturnEitherMode] {
    protected def mode[G <: MethodConstraint] = new ReturnEitherMode[G]
  }*/

  object returnResult extends Mode.Import[ReturnResultMode] {
    protected def mode[G <: MethodConstraint] = new ReturnResultMode[G]
  }

  object returnTry extends Mode.Import[ReturnTryMode] {
    protected def mode[G <: MethodConstraint] = new ReturnTryMode[G]
  }

  object exponentialBackoff extends Mode.Import[ExponentialBackoffMode] {
    protected def mode[G <: MethodConstraint] = new ExponentialBackoffMode[G]()
  }

  object keepCalmAndCarryOn extends Mode.Import[KeepCalmAndCarryOnMode] {
    protected def mode[G <: MethodConstraint] = new KeepCalmAndCarryOnMode[G]()
  }

  object returnOption extends Mode.Import[ReturnOptionMode] {
    protected def mode[G <: MethodConstraint] = new ReturnOptionMode[G]()
  }

  object returnFuture {
    implicit def modeImplicit[G <: MethodConstraint](implicit ec: ExecutionContext) =
      new ReturnFutureMode[G]

    def apply[G <: MethodConstraint](implicit ec: ExecutionContext) = modeImplicit[G]
  }

  object timeExecution {
    implicit def modeImplicit[D: TimeSystem.ByDuration, G <: MethodConstraint] =
      new TimeExecution[D, G]

    def apply[D: TimeSystem.ByDuration, G <: MethodConstraint] = modeImplicit[D, G]
  }

  class Explicitly[+Res, E <: Exception](blk: => Res) {
    def get: Res = blk
    def opt: Option[Res] = returnOption[Nothing].wrap(blk)
    def getOrElse[Res2 >: Res](t: Res2): Res2 = opt.getOrElse(blk)
    //def either: Either[E, Res] = returnEither[Nothing].wrap(blk)
    def attempt: Try[Res] = returnTry[Nothing].wrap(blk)
    def backoff(maxRetries: Int = 10, initialPause: Long = 1000L, backoffRate: Double = 2.0): Res =
      new ExponentialBackoffMode(maxRetries, initialPause, backoffRate).wrap(blk)
    def time[D: TimeSystem.ByDuration] = timeExecution[D, Nothing].wrap(blk)
    def future(implicit ec: ExecutionContext): Future[Res] = returnFuture[Nothing].wrap(blk)

    override def toString = "<unevaluated result>"
  }

}

private[core] trait Mode_1 {
  implicit def defaultMode: ThrowExceptionsMode[Nothing] = new ThrowExceptionsMode
}

private[core] class ThrowExceptionsMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = T2 forSome { type T2 <: T  }
  def wrap[T, E <: Exception](t: => T): T = t
  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value
}

private[core] class ExplicitMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = modes.Explicitly[T, E]

  def wrap[T, E <: Exception](t: => T): modes.Explicitly[T, E] =
    new modes.Explicitly[T, E](t)

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value.get
}

private[core] class ReturnTryMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = Try[T]
  def wrap[T, E <: Exception](t: => T): Try[T] = Try(t)

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value.get

  override def toString = "[modes.returnTry]"
}

private[core] class ExponentialBackoffMode[+G <: MethodConstraint](maxRetries: Int = 10,
                                                                   initialPause: Long = 1000L,
                                                                   backoffRate: Double = 2.0)
    extends Mode[G] {
  type Wrap[+T, E <: Exception] = T2 forSome { type T2 <: T }
  def wrap[T, E <: Exception](t: => T): T = {
    var multiplier = 1.0
    var count = 1
    var result: T = null.asInstanceOf[T]
    var exception: Exception = null.asInstanceOf[Exception]
    while (result == null && count < maxRetries) try { result = t } catch {
      case e: Exception =>
        exception = e
        import timeSystems.numeric._
        Thread.sleep((multiplier * initialPause).toLong)
        multiplier *= backoffRate
        count += 1
    }
    if (result != null) result else throw exception
  }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value
}

private[core] class KeepCalmAndCarryOnMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = T2 forSome { type T2 <: T }
  def wrap[T, E <: Exception](t: => T): T =
    try t
    catch { case e: Exception => null.asInstanceOf[T] }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = Option[Return](value).get

  override def toString = "[modes.kcaco]"
}

private[core] class ReturnOptionMode[+G <: MethodConstraint] extends Mode[G] {
  type Wrap[+T, E <: Exception] = Option[T]
  def wrap[T, E <: Exception](t: => T): Option[T] =
    try Some(t)
    catch { case e: Exception => None }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value.get

  override def toString = "[modes.returnOption]"
}

private[core] class ReturnFutureMode[+G <: MethodConstraint](implicit ec: ExecutionContext) extends Mode[G] {
  type Wrap[+T, E <: Exception] = Future[T]
  def wrap[T, E <: Exception](t: => T): Future[T] = Future { t }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return =
    Await.result(value, duration.Duration.Inf)

  override def flatWrap[Res, E <: Exception: ClassTag](blk: => Wrap[Res, E]): Wrap[Res, E] = blk

  override def toString = "[modes.returnFuture]"
}

private[core] class TimeExecution[D: TimeSystem.ByDuration, +G <: MethodConstraint] extends Mode[G] {
  val ts = ?[TimeSystem.ByDuration[D]]
  type Wrap[+T, E <: Exception] = (T, D)
  def wrap[T, E <: Exception](r: => T): (T, D) = {
    val t0 = System.currentTimeMillis
    (r, ts.duration(t0, System.currentTimeMillis))
  }

  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value._1

  override def toString = "[modes.timeExecution]"
}
