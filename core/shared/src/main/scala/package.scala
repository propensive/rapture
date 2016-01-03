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

import language.higherKinds
import language.experimental.macros
import reflect.runtime._
import reflect.runtime.universe._
import reflect.ClassTag

object `package` {

  type CanBuildFrom[-From, -Elem, +To] = collection.generic.CanBuildFrom[From, Elem, To]

  def alloc[T] = new AllocApply[T](0)

  def each[E <: Exception] = EachUnapplied[E]()

  implicit class EnrichedString(val string: String) extends AnyVal {
    def as[T](implicit parser: StringParser[T], mode: Mode[`String#as`]): mode.Wrap[T, parser.Throws] =
      parser.parse(string, mode)
  }

  def indentTree(s: String): String = {
    var indent = 0
    s flatMap {
      case '(' => indent += 1; s"(\n${"  "*indent}"
      case ')' => indent -= 1; s"\n${"  "*indent})"
      case ',' => s",\n${"  "*(indent - 1)}"
      case ' ' => ""
      case o => o.toString
    }
  }

  private val parseExceptionTypeTag = implicitly[TypeTag[ParseException]]

  implicit class EnrichedCollection[Coll[X] <: Seq[X]](val coll: Coll[String]) extends AnyVal {
    def mapAs[T](implicit parser: StringParser[T], cbf: CanBuildFrom[Coll[String], T, Coll[T]], mode: Mode[`Seq#mapAs`]):
        mode.Wrap[Coll[T], parser.Throws] = mode.wrap[Coll[T], parser.Throws] {
      val b = cbf(coll)
      coll foreach { x => b += mode.unwrap(parser.parse(x, mode)) }
      b.result
    }// (parseExceptionTypeTag)
  }

  private[rapture] type implicitNotFound = annotation.implicitNotFound

  private[rapture] implicit val implicitConversions: languageFeature.implicitConversions =
    language.implicitConversions

  @inline
  final def ?[T](implicit t: T) = t

  def modally[G <: MethodConstraint, E <: Exception] = new Modal[G, E]

  def yCombinator[A, B](fn: (A => B) => (A => B)): A => B = fn(yCombinator(fn))(_)

  /** Times how long it takes to perform an operation, returning a pair of the result and the
    * duration of the operation in milliseconds. */
  def time[T, D: TimeSystem.ByDuration](blk: => T): (T, D) = {
    val t = System.currentTimeMillis
    (blk, ?[TimeSystem.ByDuration[D]].duration(t, System.currentTimeMillis))
  }
 
  def enumerateMembers[T] = new Enumerator[T]
  
  @inline
  implicit class SeqExtras[A, C[A] <: Seq[A]](val xs: C[A]) {

    /** Inserts an element between each of the elements of the sequence. */
    def intersperse[B >: A, That](between: B)(implicit bf: CanBuildFrom[C[A], B, That]): That = {
      val b = bf(xs)
      xs.init foreach { x =>
        b += x
        b += between
      }
      b += xs.last
      b.result
    }

    /** Inserts an element between each of the elements of the sequence, and additionally
      * prepends and affixes the sequence with `before` and `after`. */
    def intersperse[B >: A, That](before: B, between: B, after: B)(implicit bf: CanBuildFrom[C[A], B, That]): That = {
      val b = bf(xs)
      b += before
      xs.init foreach { x =>
        b += x
        b += between
      }
      b += xs.last
      b += after
      b.result
    }

    /** Convenience method for zipping a sequence with a value derived from each element. */
    def zipWith[T](fn: A => T)(implicit bf: CanBuildFrom[C[A], (A, T), C[(A, T)]]): C[(A, T)] = {
      val b = bf(xs)
      xs.foreach { x => b += ((x, fn(x))) }
      b.result
    }
  }

  implicit class EnrichedCollectionCompanion[+C[X] <: collection.GenTraversable[X]](
      val cc: collection.generic.GenericCompanion[C]) extends AnyVal {
    def strap[T](xs: Strapped[T]*): C[T] = {
      val b = cc.newBuilder[T]
      xs foreach { b ++= _.elems }
      b.result()
    }
  }

  implicit class EnrichedArrayCompanion(val arr: Array.type) extends AnyVal {
    def strap[T: ClassTag](xs: Strapped[T]*): Array[T] = {
      val b = Array.newBuilder[T]
      xs foreach { b ++= _.elems }
      b.result()
    }
  }

  implicit class EitherExtras[L, R](either: Either[L, R]) {
    def bimap[T](leftFn: L => T, rightFn: R => T) = either match {
      case Left(left) => leftFn(left)
      case Right(right) => rightFn(right)
    }
  }
}

trait `Seq#mapAs` extends MethodConstraint
trait `String#as` extends MethodConstraint

private[core] object Strapped {
  implicit def toStrapped[T](t: T): Strapped[T] = Strapped(List(t))
  implicit def toStrapped[T](elems: Iterable[T]): Strapped[T] = Strapped(elems)
  implicit def toStrapped[T](opt: Option[T]): Strapped[T] = Strapped(opt.toList)
}

private[core] case class Strapped[+T](elems: Iterable[T]) extends AnyVal

private[core] class Enumerator[T] {
  def apply[Cls](value: Cls): List[T] = macro CoreMacros.enumerateMacro[Cls, T]
}

private[core] class Modal[G <: MethodConstraint, E <: Exception] {
  def apply[T](fn: => T)(implicit mode: Mode[G], typeTag: TypeTag[E]): mode.Wrap[T, E] = mode.wrap(fn)
}

