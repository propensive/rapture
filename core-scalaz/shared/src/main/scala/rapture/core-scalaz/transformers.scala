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

package rapture.core.scalazInterop

import rapture.core.{Errata, NotMatchingFilter, Result}

import scala.reflect.ClassTag
import scalaz.{Functor, _}

import language.higherKinds

/**
 * ResultT monad transformer
 *
 * Represents a computation of type `Result[A,B]`.
 *
 * Example:
 * {{{
 * val x: Option[Result[String, E]] = Some(Answer(1))
 * ResultT(x).map(1+).run // Some(Answer(2))
 * }}}
 **/
sealed trait ResultT[F[_], T, E <: Exception] {

  val run: F[Result[T, E]]

  /** Map on the answer of this result. */
  def map[C](f: T => C)(implicit functor: Functor[F], cte: ClassTag[E]): ResultT[F, C, E] =
    ResultT(functor.map(run)(_.map(f)))

  /** Bind through the answer of this result accumulating errors in the contents and type signature. */
  def flatMap[C, E2 <: Exception](f: T => ResultT[F, C, E2])(implicit monad: Monad[F], cte: ClassTag[E2]): ResultT[F, C, E with E2] = {
    ResultT(monad.bind[Result[T, E], Result[C, E with E2]](run) { result =>
      result.fold[F[Result[C, E with E2]]]({ a =>
        monad.map(f(a).run)( r2 => Result[C, E with E2](r2.get, r2.errors ++ result.errors))
      },{ e =>
        monad.point(Errata[C, E with E2](e))
      })
    })
  }

  /** Filter on the answer of this result. */
  def filter(p: T => Boolean)(implicit functor: Functor[F], cte: ClassTag[E]): ResultT[F, T, E with NotMatchingFilter] =
    ResultT(functor.map(run)(_.filter(p)))

  /** Alias for `filter` */
  def withFilter(p: T => Boolean)(implicit functor: Functor[F], cte: ClassTag[E]): ResultT[F, T, E with NotMatchingFilter] =
    filter(p)

}

object ResultT extends ResultTFunctions {
  /** Construct a result value. */
  def apply[F[_], T, E <: Exception : ClassTag](a: F[Result[T, E]]): ResultT[F, T, E] =
    resultT[F, T, E](a)

  /** Construct an answer value. */
  def answer[F[_], T, E <: Exception : ClassTag](a: F[T])(implicit functor: Functor[F]): ResultT[F, T, E] =
    apply[F, T, E](functor.map(a)(Result.answer[T, E]))

  /** Construct an errata value. */
  def errata[F[_], T, E <: Exception : ClassTag](a: F[E])(implicit functor: Functor[F]): ResultT[F, T, E] =
    apply[F, T, E](functor.map(a)(Result.errata[T, E]))

}

private[scalazInterop] trait ResultTFunctions {
  def resultT[F[_], T, E <: Exception](a: F[Result[T, E]]): ResultT[F, T, E] = new ResultT[F, T, E] {
    val run = a
  }
}

