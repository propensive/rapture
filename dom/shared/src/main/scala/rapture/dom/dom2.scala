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

  trait Content[Return] {
    type Type
    
    def value: Type

    def returnValue(empty: Node.Empty, elements: Seq[Type]): Return
  }

  case class AttributeContent(attribute: Attribute) extends Content[Node.Attributed] {
   
    type Type = Attribute
    
    def value: Attribute = attribute
    
    def returnValue(empty: Node.Empty, attributes: Seq[Attribute]): Node.Attributed =
      Node.Attributed(empty.name, attributes)
  }
  case class DomContent(nodes: Seq[Node.DomNode]) extends Content[Node.Full] {
    
    type Type = Seq[Node.DomNode]

    def value: Seq[Node.DomNode] = nodes

    def returnValue(empty: Node.Empty, nodes: Seq[Seq[Node.DomNode]]): Node.Full =
      Node.Full(empty.name, Nil, nodes.flatten)
  }
  
  implicit def embedNodes[From](from: From)(implicit embeddable: Embeddable[From]): DomContent =
    DomContent(embeddable.embed(from))
    
  implicit def embedAttributes(from: Attribute): AttributeContent =
    AttributeContent(from)
    

  object Embeddable {

    def of[T: Embeddable]: Embeddable[T] = implicitly[Embeddable[T]]
  
    implicit def domNodeEmbeddable: Embeddable[Node.DomNode] =
      new Embeddable[Node.DomNode] {
        def embed(from: Node.DomNode) = Seq(from)
        
      }
  
    implicit def stringEmbeddable[S: StringSerializer] =
      new Embeddable[S] {
        def embed(from: S) = Seq(Node.Text(implicitly[StringSerializer[S]].serialize(from)))
        
      }
  }

  trait Embeddable[-From] {
    def embed(from: From): Seq[Node.DomNode]
  }

  case class Attribute(key: String, value: String) {
    override def toString = s"""$key="$value""""
  }

  object Node {
    
    sealed trait DomNode extends Product with Serializable

    sealed trait Element extends DomNode {
      def name: String
      def attributes: Seq[Attribute]
      def children: Seq[DomNode]
      
      override def toString = {
        val atts = if(attributes.isEmpty) "" else attributes.mkString(" ", " ", "")
	s"<$name$atts>${children.mkString}</$name>"
      }
    }

    case class Empty(name: String) extends Element {
      
      def apply[Return](contents: Content[Return]*): Return = {
	val head = contents.head
	head.returnValue(this, contents.map(_.value.asInstanceOf[head.Type]))
      }
      
      def children = Seq[DomNode]()
      def attributes = Seq()

      override def toString = s"<$name/>"
    }
    
    case class Attributed(name: String, attributes: Seq[Attribute]) extends Element {
      
      def children = Seq[DomNode]()
      def apply(content: DomContent*): Full = Full(name, attributes, content.flatMap(_.nodes))
    }

    case class Full(name: String, attributes: Seq[Attribute], children: Seq[DomNode]) extends Element

    case class Text(content: String) extends DomNode {
      override def toString = content
    }

  }

}

object Dom2Test {
  import Dom2._

  val Table: Node.Empty = Node.Empty("table")
  val Tbody: Node.Empty = Node.Empty("tbody")
  val Tr: Node.Empty = Node.Empty("tr")
  val Td: Node.Empty = Node.Empty("td")

  val att = Attribute("key", "value")

  val tab1 = Table(Tbody(att), Tr(att))
  val tab2 = Table(1, Tbody(att)(Tr, Tr(att), Tr(Td)))
  val tab3 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td("Hello world"))))


}
