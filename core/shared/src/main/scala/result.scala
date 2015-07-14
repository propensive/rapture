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

import scala.reflect._
import scala.reflect.ClassTag

import language.experimental.macros
import language.existentials

object Result {
  private[core] def apply[T, E <: Exception](result: => T, errors: Seq[(ClassTag[_], (String, Exception))]) = try {
    if(errors.isEmpty) Answer[T, E](result) else Errata[T, E](errors)
  } catch { case e: Throwable => if(errors.isEmpty) Unforeseen[T, E](e) else Errata[T, E](errors) }

  def apply[T](result: => T): Result[T, Nothing] =
    try Answer[T, Nothing](result) catch { case e: Throwable => Unforeseen[T, Nothing](e) }

  def catching[E <: Exception]: Catching[E] = new Catching[E]()  
}

class Catching[E <: Exception]() {
  def apply[T](blk: => T)(implicit classTag: ClassTag[E]): Result[T, E] = try Answer(blk) catch {
    case e: E => Errata(Vector((?[ClassTag[E]], ("", e))))
    case e: Throwable => Unforeseen(e)
  }
}

sealed class Result[+T, E <: Exception](val answer: T, val errors: Seq[(ClassTag[_], (String, Exception))],
    val unforeseen: Option[Throwable] = None) {
 
  def errata[E2 >: E: ClassTag]: Seq[E2] =
    errors.filter(_._1 == ?[ClassTag[E2]]).map(_._2.asInstanceOf[E2])
 
  def exceptions: Seq[Exception] = errors.map(_._2._2)

  def get: T = {
    unforeseen.foreach(throw _)
    errors.foreach { case (k, (p, e)) => throw e }
    answer
  }

  def flatMap[T2, E2 <: Exception](fn: T => Result[T2, E2]): Result[T2, E with E2] =
    try {
      val res = fn(answer)
      val probs = res.errors ++ errors
      Result[T2, E with E2](res.get, probs)
    } catch {
      case e: NullPointerException if !errors.isEmpty => Errata[T2, E with E2](errors)
      case e: Throwable => Unforeseen[T2, E with E2](e)
    }

  def map[T2](fn: T => T2) = Result[T2, E](fn(answer), errors)

  def resolve[E2, T2 >: T](handlers: Each[E2, T2]*)(implicit ev: E2 <:< E): Resolved[T2, Nothing] = this match {
    case Unforeseen(e) =>
      Unforeseen(e)
    case Answer(a) =>
      Answer(a)
    case Errata((t, (_, err)) +: _) =>
      Answer(handlers.find { case Each(fn, ct) => ct == t }.get.fn(err.asInstanceOf[E2]))
  }

  def reconcile[E2, E3 <: Exception](handlers: Each[E2, E3]*)(implicit ev: E2 <:< E) = {
    val hs = handlers.map { case Each(e, typ) => typ -> e }.toMap[ClassTag[_], E2 => E3]
    errors.map { case (t, (p, e)) => hs(t)(e.asInstanceOf[E2]) }
  }
}

object Resolved {
  def unapply[T, E <: Exception](res: Result[T, E]): Option[(T, Option[Throwable])] =
    Some(res.answer -> res.unforeseen)

  def apply[T, E <: Exception](answer: T, unforeseen: Option[E]) = if(unforeseen == None) Answer(answer) else Unforeseen(unforeseen.get)
}
sealed class Resolved[+T, E <: Exception](answer: T, unforeseen: Option[Throwable])
    extends Result[T, E](answer, Seq(), unforeseen) {

  override def equals(that: Any) = that match {
    case that: Resolved[_, _] => that.answer == answer && that.unforeseen == unforeseen
    case _ => false
  }

  override def hashCode = answer.hashCode ^ unforeseen.hashCode

}

case class Answer[T, E <: Exception](override val answer: T) extends Resolved[T, E](answer, None)

case class Errata[T, E <: Exception](override val errors: Seq[(ClassTag[_], (String, Exception))]) extends
    Result[T, E](null.asInstanceOf[T], errors) {
  override def toString = "Errata(\n"+(errors.map { case (t, (p, e)) => s"$t: ${e.getMessage} [$p]" })+"\n)"
}

case class Unforeseen[T, E <: Exception](e: Throwable) extends Resolved[T, E](null.asInstanceOf[T], Some(e))

case class AbortException() extends Exception

private[core] class ReturnResultMode[+Group <: MethodConstraint] extends Mode[Group] {
  type Wrap[+R, E <: Exception] = Result[R, E]
  
  def wrap[R, E <: Exception](blk: => R): Result[R, E] = {
    try {
      val res = blk
      Result(res, accumulated)
    } catch {
      case AbortException() =>
        Result(null.asInstanceOf[R], accumulated)
      case e: Throwable =>
        if(accumulated.isEmpty) Unforeseen[R, E](e)
        else Errata(accumulated)
    }
  }

  private var accumulated: Vector[(ClassTag[_], (String, Exception))] = Vector()
  override def exception[T, E <: Exception: ClassTag](e: E, continue: Boolean = true): T = {
    accumulated :+= ((?[ClassTag[E]], (callPath, e)))
    if(continue) null.asInstanceOf[T] else throw AbortException()
  }

  override def catching[E <: Exception: ClassTag, T](blk: => T) = try blk catch {
    case e: E => 
      exception(e)
    case e: Exception => 
      throw e
  }  

  override def flatWrap[R, E <: Exception: ClassTag](blk: => Wrap[R, E]): Wrap[R, E] = blk
  
  def unwrap[Return](value: => Wrap[Return, _ <: Exception]): Return = value match {
    case Answer(a) => a
    case Errata(xs) => null.asInstanceOf[Return]
    case Unforeseen(e) => throw e
    case _ => ???
  }
  
  override def toString = "[modes.returnResult]"
}

case class Each[-E, +T](fn: E => T, classTag: ClassTag[_])
case class EachUnapplied[E]() {
  def apply[R](fn: E => R)(implicit classTag: ClassTag[E]): Each[E, R] = Each(fn, classTag)
}



