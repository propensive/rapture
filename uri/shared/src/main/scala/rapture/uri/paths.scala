package rapture.uri

import language.implicitConversions

case class RelativePath(ascent: Int, elements: Vector[String]) {
  override def toString = "../"*ascent+elements.mkString("/")
    
  override def equals(that: Any) = that match {
    case RelativePath(a, es) => a == ascent && es == elements
    case _ => false
  }

  override def hashCode = ascent ^ elements.hashCode
}

object RootRelativePath {
  def parse(s: String): Option[RootRelativePath] = {
    if(s.head == '/') Some(RootRelativePath(s.tail.split("/").to[Vector]))
    else None
  }

  implicit def linkable: Linkable[RootRelativePath] = new Linkable[RootRelativePath] {
    def link(res: RootRelativePath): Link = Link(res.toString)
  }
}

case class RootRelativePath(elements: Vector[String]) {
  override def toString = elements.mkString("/", "/", "")
    
  override def equals(that: Any) = that match {
    case RootRelativePath(es) => es == elements
    case _ => false
  }

  override def hashCode = elements.hashCode
}

object Dereferenceable {
  implicit def stringSlashString: Dereferenceable[String, String, RelativePath] = new Dereferenceable[String, String, RelativePath] {
    def dereference(p1: String, p2: String): RelativePath = RelativePath(0, Vector(p2, p1))
  }
    
  implicit def relativePathSlashString[RP <: RelativePath]: Dereferenceable[RP, String, RelativePath] = new Dereferenceable[RP, String, RelativePath] {
    def dereference(p1: RP, p2: String): RelativePath = RelativePath(0, p1.elements :+ p2)
  }
    
  implicit def rootSlashString[RRP <: RootRelativePath]: Dereferenceable[RRP, String, RootRelativePath] = new Dereferenceable[RRP, String, RootRelativePath] {
    def dereference(p1: RRP, p2: String): RootRelativePath = RootRelativePath(p1.elements :+ p2)
  }
    
  implicit def rootSlashRelative[RRP <: RootRelativePath, RP <: RelativePath]: Dereferenceable[RRP, RP, RootRelativePath] = new Dereferenceable[RRP, RP, RootRelativePath] {
    def dereference(p1: RRP, p2: RP): RootRelativePath = RootRelativePath(p2.elements ++ p1.elements.drop(p2.ascent))
  }
  
  class Capability[Path](val path: Path) {
    def /[Elem, Return](elem: Elem)(implicit deref: Dereferenceable[Path, Elem, Return]): Return =
      deref.dereference(path, elem)
  }
}

trait Dereferenceable[-P1, -P2, +Return] {
  def dereference(p1: P1, p2: P2): Return
}
