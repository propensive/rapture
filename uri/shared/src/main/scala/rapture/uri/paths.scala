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

import language.implicitConversions

import scala.annotation.implicitNotFound

case class RelativePath(ascent: Int, elements: Vector[String]) {
  override def toString = if(ascent == 0 && elements.isEmpty) "." else "../"*ascent+elements.mkString("/")
    
  override def equals(that: Any) = that match {
    case RelativePath(a, es) => a == ascent && es == elements
    case _ => false
  }

  override def hashCode = ascent ^ elements.hashCode
}

object RootedPath {
  def parse(s: String): Option[RootedPath] = {
    if(s.head == '/') Some(RootedPath(s.tail.split("/").to[Vector]))
    else None
  }

  implicit def linkable: Linkable[RootedPath] = new Linkable[RootedPath] {
    def link(res: RootedPath): PathLink = PathLink(res.toString)
  }
}

object / {
  def unapply(p: RootedPath): Option[(RootedPath, String)] =
    if(p.elements.isEmpty) None
    else Some((RootedPath(p.elements.init), p.elements.last))
}

case class RootedPath(elements: Vector[String]) {
  override def toString = elements.mkString("/", "/", "")
    
  override def equals(that: Any) = that match {
    case RootedPath(es) => es == elements
    case _ => false
  }

  override def hashCode = elements.hashCode
}

object Dereferenceable {
  implicit def stringSlashString: Dereferenceable[String, String, RelativePath] = new Dereferenceable[String, String, RelativePath] {
    def dereference(p1: String, p2: String): RelativePath = RelativePath(0, Vector(p1, p2))
  }
    
  implicit def relativePathSlashString[RP <: RelativePath]: Dereferenceable[RP, String, RelativePath] = new Dereferenceable[RP, String, RelativePath] {
    def dereference(p1: RP, p2: String): RelativePath = RelativePath(p1.ascent, p1.elements :+ p2)
  }
    
  implicit def rootSlashString[RRP <: RootedPath]: Dereferenceable[RRP, String, RootedPath] = new Dereferenceable[RRP, String, RootedPath] {
    def dereference(p1: RRP, p2: String): RootedPath = RootedPath(p1.elements :+ p2)
  }
    
  implicit def rootSlashRelative[RRP <: RootedPath, RP <: RelativePath]: Dereferenceable[RRP, RP, RootedPath] = new Dereferenceable[RRP, RP, RootedPath] {
    def dereference(p1: RRP, p2: RP): RootedPath = RootedPath(p2.elements ++ p1.elements.drop(p2.ascent))
  }
  
  class Capability[Path](val path: Path) {
    def /[Elem, Return](elem: Elem)(implicit deref: Dereferenceable[Path, Elem, Return]): Return =
      deref.dereference(path, elem)
  }
}

@implicitNotFound("it is not possible to dereference a value of type ${P1} by a value of type ${P2}.")
trait Dereferenceable[-P1, -P2, +Return] {
  def dereference(p1: P1, p2: P2): Return
}

object Parentable {
  implicit def relativePathParent[RP <: RelativePath]: Parentable[RP, RelativePath] = new Parentable[RP, RelativePath] {
    def parent(p: RP): RelativePath = RelativePath(if(p.elements.length == 0) p.ascent + 1 else p.ascent, p.elements.dropRight(1))
  }
    
  implicit def rootRelativePathParent[RRP <: RootedPath]: Parentable[RRP, RootedPath] = new Parentable[RRP, RootedPath] {
    def parent(p1: RRP): RootedPath = RootedPath(p1.elements.dropRight(1))
  }
    
  class Capability[Path](val path: Path) {
    def parent[Return](implicit parentable: Parentable[Path, Return]): Return =
      parentable.parent(path)
  }
}

@implicitNotFound("type ${P} does not have a parent")
trait Parentable[-P, +Return] {
  def parent(p1: P): Return
}
