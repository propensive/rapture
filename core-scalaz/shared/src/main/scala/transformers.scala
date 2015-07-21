package rapture.core.scalazInterop

import rapture.core.{Errata, NotMatchingFilter, Result}

import scala.language.{higherKinds, reflectiveCalls}
import scala.reflect.ClassTag
import scalaz.{Functor, _}

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
sealed trait ResultT[F[+ _], +T, E <: Exception] {

  val run: F[Result[T, E]]

  /** Map on the answer of this result. */
  def map[C](f: T => C)(implicit functor: Functor[F], cte : ClassTag[E]): ResultT[F, C, E] =
    ResultT(functor.map(run)(_.map(f)))

  /** Bind through the answer of this result. */
  def flatMap[C](f: T => ResultT[F, C, E])(implicit functor: Monad[F], cte : ClassTag[E]): ResultT[F, C, E] =
    ResultT[F, C, E](functor.bind(run)(_.fold(a => f(a).run, b => functor.point(Errata[C, E](b)))))

  /** Filter on the answer of this result. */
  def filter(p: T => Boolean)(implicit functor: Functor[F], cte : ClassTag[E]): ResultT[F, T, E with NotMatchingFilter] =
    ResultT(functor.map(run)(_.filter(p)))

  /** Alias for `filter`.
    * @since 7.0.2
    */
  def withFilter(p: T => Boolean)(implicit functor: Functor[F], cte : ClassTag[E]): ResultT[F, T, E with NotMatchingFilter] =
    filter(p)

}

object ResultT extends ResultTFunctions {
  /** Construct a result value. */
  def apply[F[+ _], T, E <: Exception : ClassTag](a: F[Result[T, E]]): ResultT[F, T, E] =
    resultT[F, T, E](a)

  /** Construct an answer value. */
  def answer[F[+ _], T, E <: Exception : ClassTag](a: F[T])(implicit functor: Functor[F]): ResultT[F, T, E] =
    apply[F, T, E](functor.map(a)(Result.answer[T, E]))

  /** Construct an errata value. */
  def errata[F[+ _], T, E <: Exception : ClassTag](a: F[E])(implicit functor: Functor[F]): ResultT[F, T, E] =
    apply[F, T, E](functor.map(a)(Result.errata[T, E]))

}

private[scalazInterop] trait ResultTFunctions {
  def resultT[F[+ _], T, E <: Exception](a: F[Result[T, E]]): ResultT[F, T, E] = new ResultT[F, T, E] {
    val run = a
  }
}

