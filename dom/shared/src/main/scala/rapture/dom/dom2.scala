package rapture.dom2

import language.implicitConversions
import language.higherKinds
import language.existentials

trait One
trait Two
trait Three
trait Four
trait Five

object Dom2 {

  trait Content[Return] {
    type Elem
    
    def value: Elem

    def returnValue(empty: Node.Empty, elements: Seq[Elem]): Return
  }

  case class AttributeContent(attribute: Attribute) extends Content[Node.Attributed] {
   
    type Elem = Attribute
    
    def value: Attribute = attribute
    
    def returnValue(empty: Node.Empty, attributes: Seq[Attribute]): Node.Attributed =
      Node.Attributed(empty.name, attributes)
  }
  case class DomContent(node: Node.DomNode) extends Content[Node.Full] {
    
    type Elem = Node.DomNode

    def value: Node.DomNode = node

    def returnValue(empty: Node.Empty, nodes: Seq[Node.DomNode]): Node.Full =
      Node.Full(empty.name, Nil, nodes)
  }
  
  implicit def convertNodes[From <: Node.DomNode](from: From)(implicit converter: Converter[From]): DomContent =
    DomContent(from)
    
  implicit def convertAttributes(from: Attribute): AttributeContent =
    AttributeContent(from)
    

  trait Converter[From] {
    def convert(from: From): Seq[From]
  }

  implicit def attributeConverter: Converter[Attribute] =
    new Converter[Attribute] {
      def convert(from: Attribute) = Seq(from)

    }
  
  implicit def generalConverter: Converter[Node.General] =
    new Converter[Node.General] {
      def convert(from: Node.General) = Seq(from)
      
    }
  
  implicit def emptyConverter: Converter[Node.Empty] =
    new Converter[Node.Empty] {
      def convert(from: Node.Empty) = Seq(from)
      
    }
  
  implicit def attributedConverter: Converter[Node.Attributed] =
    new Converter[Node.Attributed] {
      def convert(from: Node.Attributed) = Seq(from)
      
    }
  
  implicit def fullConverter: Converter[Node.Full] =
    new Converter[Node.Full] {
      def convert(from: Node.Full) = Seq(from)
      
    }
  
  implicit def domNodeConverter: Converter[Node.DomNode] =
    new Converter[Node.DomNode] {
      def convert(from: Node.DomNode) = Seq(from)
      
    }
  
  case class Attribute(key: String, value: String) {
    override def toString = s"""$key="$value""""
  }

  object Node {
    
    sealed trait DomNode extends Product with Serializable

    sealed trait General extends DomNode {
      def name: String
      def attributes: Seq[Attribute]
      def children: Seq[DomNode]
      
      override def toString = {
        val atts = if(attributes.isEmpty) "" else attributes.mkString(" ", " ", "")
	s"<$name$atts>${children.mkString}</$name>"
      }
    }

    case class Empty(name: String) extends General {
      
      def apply[Return](contents: Content[Return]*): Return = {
	val head = contents.head
	head.returnValue(this, contents.map(_.value.asInstanceOf[head.Elem]))
      }
      
      def children = Seq[DomNode]()
      def attributes = Seq()

      override def toString = s"<$name/>"
    }
    
    case class Attributed(name: String, attributes: Seq[Attribute]) extends General {
      
      def children = Seq[DomNode]()
      def apply(content: DomContent*): Full = Full(name, attributes, content.map(_.node))
    }

    case class Full(name: String, attributes: Seq[Attribute], children: Seq[DomNode]) extends General

    case class StringNode(content: Seq[String]) extends DomNode

  }

}

object Dom2Test {
  import Dom2._

  val Table: Node.Empty = Node.Empty("table")
  val Tbody: Node.Empty = Node.Empty("tbody")
  val Tr: Node.Empty = Node.Empty("tr")
  val Td: Node.Empty = Node.Empty("td")

  val att = Attribute("key", "value")

  //val tab1 = Table(Tbody(att), Tr(att))
  //val tab2 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td)))
  //val tab3 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td("Hello world"))))


}
