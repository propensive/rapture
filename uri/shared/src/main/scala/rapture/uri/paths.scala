package rapture.uri

import language.implicitConversions

object New {

  object Linkable {
    implicit val relativePathLinkable: Linkable[RelativePath] =
      new Linkable[RelativePath] { def link(rp: RelativePath): String = rp.toString }
    
    implicit val rootRelativePathLinkable: Linkable[RootRelativePath] =
      new Linkable[RootRelativePath] { def link(rrp: RootRelativePath): String = rrp.toString }
  }

  trait Linkable[T] { def link(t: T): String }

  case class RelativePath(ascent: Int, elems: List[String]) {
    override def toString = "../"*ascent+elems.reverse.mkString("/")
    
    override def equals(that: Any) = that match {
      case RelativePath(a, es) => a == ascent && es == elems
      case _ => false
    }

    override def hashCode = ascent ^ elems.hashCode
  }

  case class RootRelativePath(elems: List[String]) {
    override def toString = elems.reverse.mkString("/", "/", "")
    
    override def equals(that: Any) = that match {
      case RootRelativePath(es) => es == elems
      case _ => false
    }

    override def hashCode = elems.hashCode
  }

  object ^ extends RootRelativePath(Nil)
  
  val ^^ = ".."

  object Dereferenceable {
    implicit def stringSlashString: Dereferenceable[String, String, RelativePath] = new Dereferenceable[String, String, RelativePath] {
      def dereference(p1: String, p2: String): RelativePath = RelativePath(0, List(p2, p1))
    }
    
    implicit def relativePathSlashString[RP <: RelativePath]: Dereferenceable[RP, String, RelativePath] = new Dereferenceable[RP, String, RelativePath] {
      def dereference(p1: RP, p2: String): RelativePath = RelativePath(0, p2 :: p1.elems)
    }
    
    implicit def rootSlashString[RRP <: RootRelativePath]: Dereferenceable[RRP, String, RootRelativePath] = new Dereferenceable[RRP, String, RootRelativePath] {
      def dereference(p1: RRP, p2: String): RootRelativePath = RootRelativePath(p2 :: p1.elems)
    }
    
    implicit def rootSlashRelative[RRP <: RootRelativePath, RP <: RelativePath]: Dereferenceable[RRP, RP, RootRelativePath] = new Dereferenceable[RRP, RP, RootRelativePath] {
      def dereference(p1: RRP, p2: RP): RootRelativePath = RootRelativePath(p2.elems ::: p1.elems.drop(p2.ascent))
    }
  }

  abstract class Dereferenceable[-P1, -P2, +Return] {
    def dereference(p1: P1, p2: P2): Return
  }

  implicit def dereference[Path](path: Path): Dereference[Path] = new Dereference[Path](path)

  class Dereference[Path](val path: Path) {
    def /[Elem, Return](elem: Elem)(implicit deref: Dereferenceable[Path, Elem, Return]): Return =
      deref.dereference(path, elem)
  }

  

}

object Testing {

  import New._

  val a = "foo" / "bar"
  val b = "foo" / "bar" / "baz"

  val c = ^ / "foo"
  val d = ^ / "foo" / "bar"

  val e = ^ / a
  val f = ^ / "foo" / a

}
