package rapture.dom2

import rapture.core._

import language.implicitConversions
import language.higherKinds
import language.existentials

trait One
trait Two
trait Three
trait Four
trait Five

object Dom2 {

  trait Content[That, This, Child, Return] {
    type Position
    
    def value: Position

    def returnValue(empty: Node.Empty[That, This], elements: Seq[Position]): Return
  }

  case class AttributeContent[That, This, Child](
    attribute: Attribute
  ) extends Content[That, This, Child, Node.Attributed[That, This]] {
   
    type Position = Attribute
    
    def value: Attribute = attribute
    
    def returnValue(
      empty: Node.Empty[That, This],
      attributes: Seq[Attribute]
    ): Node.Attributed[That, This] =
      Node.Attributed(empty.name, attributes)
  }

  case class DomContent[That, This, Child](
    nodes: Seq[Node.DomNode[This, Child]]
  ) extends Content[That, This, Child, Node.Full[That, This]] {
    
    type Position = Seq[Node.DomNode[This, Child]]

    def value: Seq[Node.DomNode[This, Child]] = nodes

    def returnValue(
      empty: Node.Empty[That, This],
      nodes: Seq[Seq[Node.DomNode[This, Child]]]
    ): Node.Full[That, This] =
      Node.Full(empty.name, Nil, nodes.flatten)
  }
  
  implicit def embedNodes[From, That, This, Child](from: From)(implicit embeddable: Embeddable[From, This, Child]): DomContent[That, This, Child] =
    DomContent(embeddable.embed(from))
    
  implicit def embedAttributes[That, This, Child](from: Attribute): AttributeContent[That, This, Child] =
    AttributeContent(from)
    

  object Embeddable {

    implicit def domNodeEmbeddable[Type, Child]: Embeddable[Node.DomNode[Type, Child], Type, Child] =
      new Embeddable[Node.DomNode[Type, Child], Type, Child] { def embed(from: Node.DomNode[Type, Child]) = Seq(from) }
  
    implicit def stringEmbeddable[S: StringSerializer] =
      new Embeddable[S, String, String] {
        def embed(from: S) = Seq(Node.Text(implicitly[StringSerializer[S]].serialize(from)))
      }
  }

  trait Embeddable[-From, Type, Child] {
    def embed(from: From): Seq[Node.DomNode[Type, Child]]
  }

  case class Attribute(key: String, value: String) {
    override def toString = s"""$key="$value""""
  }

  object Node {
    
    sealed trait DomNode[This, Child] extends Product with Serializable

    sealed trait Element[This, Child] extends DomNode[This, Child] {
      def name: String
      def attributes: Seq[Attribute]
      def children: Seq[DomNode[Child, _]]
      
      override def toString = {
        val atts = if(attributes.isEmpty) "" else attributes.mkString(" ", " ", "")
	s"<$name$atts>${children.mkString}</$name>"
      }
    }

    case class Empty[This, Child](name: String) extends Element[This, Child] {
      
      def apply[Return](contents: Content[This, Child, _, Return]*): Return = {
	val head = contents.head
	head.returnValue(this, contents.map(_.value.asInstanceOf[head.Position]))
      }
      
      def children = Seq[DomNode[Child, _]]()
      def attributes = Seq()

      override def toString = s"<$name/>"
    }
    
    case class Attributed[This, Child](name: String, attributes: Seq[Attribute]) extends Element[This, Child] {
      
      def children = Seq[DomNode[Child, _]]()
      
      def apply[Child2](content: DomContent[This, Child, Child2]*): Full[This, Child] =
        Full(name, attributes, content.flatMap(_.nodes))
    }

    case class Full[This, Child](
      name: String,
      attributes: Seq[Attribute],
      children: Seq[DomNode[Child, _]]
    ) extends Element[This, Child]

    case class Text(content: String) extends DomNode[String, String] {
      override def toString = content
    }

  }

}

object Dom2Test {
  import Dom2._

  val Table: Node.Empty[String, Symbol] = Node.Empty("table")
  val Tbody: Node.Empty[Symbol, Int] = Node.Empty("tbody")
  val Tr: Node.Empty[Int, Boolean] = Node.Empty("tr")
  val Td: Node.Empty[Boolean, String] = Node.Empty("td")

  val att = Attribute("key", "value")

  val tab1: Node.Full[String, Symbol] = Table(Tbody(Tr(Td, Td(att))))
  //val tab2 = Table(1, Tbody(att)(Tr, Tr(att), Tr(Td)))
  //val tab3 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td("Hello world"))))


}
