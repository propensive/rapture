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

package rapture.uri
import rapture.core._

trait `Navigable#children` extends MethodConstraint
trait `Navigable#descendants` extends MethodConstraint
trait `Navigable#isDirectory` extends MethodConstraint
trait `Navigable#walkFilter` extends MethodConstraint

trait Navigable[UrlType] {

  def children(url: UrlType)(implicit mode: Mode[`Navigable#children`]
  ): mode.Wrap[Seq[UrlType], Exception]

  /** Returns false if the filesystem object represented by this FsUrl is a file, and true if
    * it is a directory. */
  def isDirectory(url: UrlType)(implicit mode: Mode[`Navigable#isDirectory`]
  ): mode.Wrap[Boolean, Exception]

  /** If this represents a directory, returns an iterator over all its descendants,
    * otherwise returns the empty iterator. */
  def descendants(url: UrlType)(implicit mode: Mode[`Navigable#descendants`]
  ): mode.Wrap[Iterator[UrlType], Exception] =
    mode wrap {
      children(url)(modes.throwExceptions()).iterator.flatMap { c =>
        if (isDirectory(c)(modes.throwExceptions()))
          Iterator(c) ++ descendants(c)(modes.throwExceptions())
        else Iterator(c)
      }
    }
}

class NavigableExtras[UrlType : Navigable](url: UrlType) {

  /** Return a sequence of children of this URL */
  def children(implicit mode: Mode[`Navigable#children`]) =
    mode flatWrap ?[Navigable[UrlType]].children(url)

  /** Return true if this URL node is a directory (i.e. it can contain other URLs). */
  def isDirectory(implicit mode: Mode[`Navigable#isDirectory`]
      ): mode.Wrap[Boolean, Exception] =
    mode flatWrap ?[Navigable[UrlType]].isDirectory(url)

  /** Return an iterator of all descendants of this URL. */
  def descendants(implicit mode: Mode[`Navigable#descendants`]
      ): mode.Wrap[Iterator[UrlType], Exception] =
    mode flatWrap ?[Navigable[UrlType]].descendants(url)

  def walkFilter(cond: UrlType => Boolean)(
      implicit mode: Mode[`Navigable#walkFilter`]
      ): mode.Wrap[Seq[UrlType], Exception] = mode wrap {
    children(modes.throwExceptions()) filter cond flatMap { f =>
      new NavigableExtras(f).walkFilter(cond)(modes.throwExceptions())
    }
  }
}
