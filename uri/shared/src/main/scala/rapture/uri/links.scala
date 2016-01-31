/******************************************************************************************************************\
* Rapture URI, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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

package rapture.uri
import rapture.core._

import language.experimental.macros

import scala.collection.mutable.WrappedArray

/** Repenesents a URI scheme */
trait Scheme[+U <: Uri] {
  def schemeName: String
  override def toString() = schemeName
}


object / {
  def unapply(p: SimplePath): Option[(SimplePath, String)] =
    if(p.isRoot) None else Some((p.init, p.last))
}

/** Represents an absolute (i.e. relative to a canonical base) path. */
abstract class AbsolutePath[+PathType <: AbsolutePath[PathType]](elements: Seq[String],
    afterPath: AfterPath) extends Path[PathType](0, elements, afterPath) { path =>

  /** This is an absolute path */
  override def absolute = true

  /** Constructs a new path */
  def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): PathType

  /** Returns true if this is a root path */
  def isRoot = elements.isEmpty

  /** Returns the first element of the path, if it exists */
  def head = path.elements.head

  /** Returns the last element of the path, if it exists */
  def last = path.elements.last

  /** Returns all but the first element of the path */
  def tail = dropRight(1)

  /** Returns all but the last element of the path */
  def init = drop(1)

  /** Returns the parent path of this path; equivalent to `tail` */
  def parent: PathType = drop(1)

  /** Constructs a new path by appending the specified path element to this path */
  override def /(p: String): PathType = makePath(0, path.elements ++ Array(p), afterPath)

  override def toString() = pathString

  def pathString = elements.mkString("/", "/", "")+afterPathString

  /** Drops the specified number of path elements from the left of the path */
  def drop(n: Int): PathType =
    if(path.elements.length - n <= 0) makePath(0, Nil, afterPath)
    else makePath(0, path.elements.dropRight(n), afterPath)

  /** Drops the specified number of path elements from the right of the path */
  def dropRight(n: Int): PathType =
    if(path.elements.length - n <= 0) makePath(0, Nil, afterPath)
    else makePath(0, path.elements.drop(n), afterPath)

  /** Drops all but the specified number of path elements from the left of the path */
  def take(n: Int): PathType =
    if(path.elements.length - n <= 0) makePath(0, Nil, afterPath)
    else makePath(0, path.elements.take(n), afterPath)

  /** Constructs a new path by following the relative link from this path */
  def +[P <: Path[P]](dest: P): Path[_] =
    makePath(0, path.elements.drop(dest.ascent) ++ dest.elements, afterPath)

  def link[P <: AbsolutePath[P]](dest: P)(implicit linkable: Linkable[PathType, P]) =
    linkable.link(this.asInstanceOf[PathType], dest)

  def -[P <: AbsolutePath[P]](src: P)(implicit linkable: Linkable[P, PathType]) =
    linkable.link(src, this.asInstanceOf[PathType])

}

/** Companion object for simple paths, including a method for creating a path from a `String` */
object SimplePath {
  def parse(path: String): SimplePath =
    if(path.startsWith("/")) parse(path.substring(1))
    else new SimplePath(path.split("/") ++ {
      if(path.endsWith("/")) Array("")
      else Array[String]()
    }, Map())
}

/** Defines a very simple absolute path with an unspecified base
  *
  * @param elements The path elements which make up this absolute path */
class SimplePath(elements: Seq[String], afterPath: AfterPath) extends AbsolutePath[SimplePath](
    elements, afterPath) {
  def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath) =
    new SimplePath(elements, afterPath)
}

class RelativePath(ascent: Int, elements: Seq[String], afterPath: AfterPath)
    extends Path[RelativePath](ascent, elements, afterPath) {
  def makePath(a: Int, e: Seq[String], ap: AfterPath): RelativePath = new RelativePath(a, e, ap)
}

object QueryType {
  implicit def caseClassParamable[T <: Product]: QueryType[Path[_], T] =
    macro UriMacros.paramsMacro[T]
}

trait QueryType[-PathType, Q] {
  def extras(existing: Map[Char, (String, Double)], q: Q): Map[Char, (String, Double)]
}

/** Represents a path which is to be considered relative to another (unspecified) path or URL
  *
  * @constructor Creates a new relative path with the specified ascent and elements
  * @param ascent The number of levels to navigate up the path hierarchy to reach the common
  *        parent
  * @param elements The `String` components of this path */
abstract class Path[+PathType <: Path[PathType]](val ascent: Int, val elements: Seq[String],
    val afterPath: AfterPath) extends Link { path =>

  /** This is a relative path */
  def absolute = false

  def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): PathType

  protected def afterPathString = afterPath.to[List] sortBy { case (k, (s, p)) => p } map {
      case (k, (s, p)) => k+s } mkString ""

  /** Adds a path component to this relative path */
  def /(s: String): PathType = makePath(ascent, Array(s) ++ elements, Map())

  @deprecated(message = "Use the query method instead.", since = "0.10.0")
  def /?[Q](q: Q)(implicit qt: QueryType[PathType, Q]) = query[Q](q)(qt)

  def query[Q](q: Q)(implicit qt: QueryType[PathType, Q]) =
    makePath(ascent, elements, qt.extras(afterPath, q))

  override def toString() =
    (if(ascent == 0 && elements.isEmpty) "."
    else if(ascent == 0 && elements.head == "" && elements.length == 1) "/"
    else (Array.fill(ascent)("..") ++ elements).mkString("/"))+afterPathString

  override def equals(that: Any) = that match {
    case p: Path[_] =>
      p.absolute == absolute && p.ascent == ascent && (p.elements.toArray[String]:
          WrappedArray[String]) == (elements.toArray[String]: WrappedArray[String])
    case _ => false
  }
}

object Link {
  val self = new Link {
    def absolute = false
    override def toString() = "."
  }

  def apply(s: String) = new Link {
    def absolute = true
    override def toString = s
  }
}

trait Link { def absolute: Boolean }

trait Uri extends Link {
  def scheme: Scheme[_]
  def schemeSpecificPart: String
  override def toString() = scheme.schemeName+":"+schemeSpecificPart
}

/** Calculates the relative link between this path and the specified destination path
  *
  * FIXME: This scales badly as path lengths increase.
  *
  * @param dest The destination path to calculate the relative link to
  * @return The calculated relative path */
object generalLink {
  def apply(src: List[String], dest: List[String]): (Int, List[String]) =
    yCombinator[(Int, List[String], List[String], List[String]), (Int, List[String])] { fn => v =>
      v match {
        case (0, Nil, x :: x2 :: xs, y :: ys) if x == y =>
          fn((0, Nil, x2 :: xs, ys))
        
        case (up, tail, x :: Nil, y :: Nil) if x == y =>
          (0, List[String]())
        
        case (up, tail, x :: Nil, y :: ys) =>
          (up, tail.reverse ::: List(y) ::: ys)
        
        case (0, _, Nil, ys) =>
          (1, List[String](dest.head))
        
        case (up, tail, Nil, ys) =>
          (up, tail.reverse ::: ys)
        
        case (up, tail, _ :: x :: xs, Nil) =>
          fn((up + 1, tail, x :: xs, Nil))

        case (up, tail, x :: Nil, Nil) =>
          fn((up, tail, Nil, Nil))

        case (up, tail, _ :: x :: xs, y :: ys) =>
          fn((up + 1, y :: tail, x :: xs, ys))

      }
    } ((0, Nil, if(src == Nil) List("") else src, if(dest == Nil) List("") else dest))
}

trait Linkable[-Src, -Dest] {
  type Result <: Link
  def link(src: Src, dest: Dest): Result
}

object SimplePathsLinkable extends Linkable[SimplePath, SimplePath] {
  type Result = RelativePath
  def link(src: SimplePath, dest: SimplePath): RelativePath = {
    val lnk = generalLink(src.elements.to[List], dest.elements.to[List])
    new RelativePath(lnk._1, lnk._2, dest.afterPath)
  }
}
