package rapture.dom2

import language.implicitConversions
import language.higherKinds
import language.existentials

trait One
trait Two extends One
trait Three extends Two
trait Four extends Three
trait Five extends Four

/*object DomTest {
  import Dom._

  object Table extends Node.Empty[One, Two]("table")
  object Tbody extends Node.Empty[Two, Three]("tbody")
  object Tr extends Node.Empty[Three, Four]("tr")
  object Td extends Node.Empty[Four, Five]("td")
  object Th extends Node.Empty[Four, Five]("th")

  def att = Attribute("key", "value")
  
  //Table(Tbody(Tr))
  //Tbody(Tr, Tr, Tr)
  //Tbody(att)(Tr, Td)
  
}


object Dom {

  implicit def conv[R[_, _], F](tag: F)(implicit apVal: Apply[F, R]): Content[F, R] =
    new Content[F, R] {
      def values = List(tag)
      def ap = apVal
    }
  
  object Apply {

    implicit def attributeApply: Apply[Attribute, Node.Attributed] =
      new Apply[Attribute, Node.Attributed] {
        
	def apply[T, C](name: String, attributes: Seq[Attribute]): Node.Attributed[T, C] =
	  Node.Attributed[T, C](name, attributes)
      }
    
    implicit def contentApply[T, C]: Apply[Node[T, C], Node.Full] =
      new Apply[Node[T, C], Node.Full] {
        
	def apply[T2, C2](name: String, nodes: Seq[Node[T, C]]): Node.Full[T2, C2] =
	  Node.Full[T2, C2](name, Nil, nodes)
      }
  }

  trait Apply[O, R[_, _]] {
    def apply[T, C](name: String, xs: Seq[O]): R[T, C]
  }

  trait Content[O, R[_, _]] {
    def values: Seq[O]
    def ap: Apply[O, R]
  }

  case class Attribute(key: String, value: String)

  trait Node[This, Children]

  object Node {
    case class Empty[This, Children](name: String) extends Node[This, Children] {
      def apply[From, R[_, _]](content: Content[From, R]*): R[This, Children] = content.head.ap.apply(name, content.flatMap(_.values))
    }
    
    case class Attributed[This, Children](name: String, attributes: Seq[Attribute]) extends Node[This, Children] {
      def apply(content: (Content[Node[This, Children], R] forSome { type R[_, _] })*): Full[This, Children] = Full(name, attributes, content)
    }

    case class Full[This, Children](name: String, attributes: Seq[Attribute], content: Seq[Any]) extends Node[This, Children]
  }
}

*/
object Dom2 {

  implicit def convert[F, T](n: F)(implicit c: Converter[F, T]): Content[F, T] = Content(c.convert(n))

  implicit def attributeConverter: Converter[Attribute, Node.Attributed] = new Converter[Attribute, Node.Attributed] {
    def convert(from: Attribute) = List(from)
  }
  
  implicit def nodeConverter[N <: Node.General]: Converter[N, Node.Full] = new Converter[N, Node.Full] {
    def convert(from: N) = List(from)
  }
  
  implicit def stringConverter: Converter[String, Node.Full] = new Converter[String, Node.Full] {
    def convert(from: String) = List(from)
  }
  
  trait Converter[From, To] { def convert(from: From): List[From] }

  case class Content[From, To](values: List[From])

  case class Attribute(key: String, value: String)

  object Node {
    
    sealed trait General

    case class Empty(name: String) extends General {
      def apply[From, To](content: Content[From, To]*): To = ???
    }
    
    case class Attributed(name: String, attributes: Seq[Attribute]) extends General {
      def apply(content: Content[General, Full]*): Full = Full(name, attributes, content.to[List])
    }

    case class Full(name: String, attributes: Seq[Attribute], content: Seq[Any]) extends General
  }

}

object Dom2Test {
  import Dom2._

  val Table: Node.Empty = Node.Empty("table")
  val Tbody: Node.Empty = Node.Empty("tbody")
  val Tr: Node.Empty = Node.Empty("tr")
  val Td: Node.Empty = Node.Empty("td")

  val att = Attribute("key", "value")

  val tab1 = Table(Tbody(Tr))
  val tab2 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td)))
  val tab3 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td("Hello world"))))
}
