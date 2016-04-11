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

  trait Content[That, ThatAtts, This, Child, Atts, Return] {
    type Position
    
    def value: Position

    def returnValue(empty: Node.Empty[That, ThatAtts, This], elements: Seq[Position]): Return
  }

  case class AttributeContent[That, ThatAtts, This, Child, Atts](
    attribute: Attribute[ThatAtts]
  ) extends Content[That, ThatAtts, This, Child, Atts, Node.Attributed[That, ThatAtts, This]] {
   
    type Position = Attribute[ThatAtts]
    
    def value: Attribute[ThatAtts] = attribute
    
    def returnValue(
      empty: Node.Empty[That, ThatAtts, This],
      attributes: Seq[Attribute[ThatAtts]]
    ): Node.Attributed[That, ThatAtts, This] =
      Node.Attributed(empty.name, attributes)
  }

  case class DomContent[That, ThatAtts, This, Child, Atts](
    nodes: Seq[Node.DomNode[This, Atts, Child]]
  ) extends Content[That, ThatAtts, This, Child, Atts, Node.Full[That, ThatAtts, This]] {
    
    type Position = Seq[Node.DomNode[This, Atts, Child]]

    def value: Seq[Node.DomNode[This, Atts, Child]] = nodes

    def returnValue(
      empty: Node.Empty[That, ThatAtts, This],
      nodes: Seq[Seq[Node.DomNode[This, Atts, Child]]]
    ): Node.Full[That, ThatAtts, This] =
      Node.Full(empty.name, Nil, nodes.flatten)
  }
  
  implicit def embedNodes[From, That, ThatAtts, This, Atts, Child](from: From)(implicit embeddable: Embeddable[From, This, Atts, Child]): DomContent[That, ThatAtts, This, Child, Atts] =
    DomContent(embeddable.embed(from))
    
  implicit def embedAttributes[That, ThatAtts, This, Child, Atts](from: Attribute[ThatAtts]): AttributeContent[That, ThatAtts, This, Child, Atts] =
    AttributeContent(from)
    

  object Embeddable extends Embeddable_1 {

    implicit def domNodeEmbeddable[Type, Atts, Child]:
        Embeddable[Node.DomNode[Type, Atts, Child], Type, Atts, Child] =
      new Embeddable[Node.DomNode[Type, Atts, Child], Type, Atts, Child] {
        def embed(from: Node.DomNode[Type, Atts, Child]) = Seq(from)
      }
  
    implicit def stringEmbeddable[Type, Atts, Child, From: StringSerializer]: Embeddable[From, Type, Atts, Child] =
      new Embeddable[From, Type, Atts, Child] {
        def embed(from: From) =
	  Seq(Node.Text(implicitly[StringSerializer[From]].serialize(from)))
      }
  }

  trait Embeddable_1 {
    //implicit def 
  }

  trait Embeddable[-From, Type, Atts, Child] {
    def embed(from: From): Seq[Node.DomNode[Type, Atts, Child]]
  }

  case class Attribute[Att](key: String, value: String) {
    override def toString = s"""$key="$value""""
  }

  object Node {
    
    sealed trait DomNode[This, Atts, Child] extends Product with Serializable

    sealed trait Element[This, Atts, Child] extends DomNode[This, Atts, Child] {
      def name: String
      def attributes: Seq[Attribute[Atts]]
      def children: Seq[DomNode[Child, _, _]]
      
      override def toString = {
        val atts = if(attributes.isEmpty) "" else attributes.mkString(" ", " ", "")
	s"<$name$atts>${children.mkString}</$name>"
      }
    }

    case class Empty[This, Atts, Child](name: String) extends Element[This, Atts, Child] {
      
      def apply[Return](contents: Content[This, Atts, Child, _, _, Return]*): Return = {
	val head = contents.head
	head.returnValue(this, contents.map(_.value.asInstanceOf[head.Position]))
      }
      
      def children = Seq[DomNode[Child, _, _]]()
      def attributes = Seq()

      override def toString = s"<$name/>"
    }
    
    case class Attributed[This, Atts, Child](
      name: String,
      attributes: Seq[Attribute[Atts]]
    ) extends Element[This, Atts, Child] {
      
      def children = Seq[DomNode[Child, _, _]]()
      
      def apply[Grandchild, ChildAtts](
        content: DomContent[This, Atts, Child, Grandchild, ChildAtts]*
      ): Full[This, Atts, Child] =
        Full(name, attributes, content.flatMap(_.nodes))
    }

    case class Full[This, Atts, Child](
      name: String,
      attributes: Seq[Attribute[Atts]],
      children: Seq[DomNode[Child, _, _]]
    ) extends Element[This, Atts, Child]

    case class Text[Type, Atts, Child](content: String) extends DomNode[Type, Atts, Child] {
      override def toString = content
    }

  }

}

trait MyAtt

object Dom2Test {
  import Dom2._

  val Table: Node.Empty[String, MyAtt, Symbol] = Node.Empty("table")
  val Tbody: Node.Empty[Symbol, MyAtt, Int] = Node.Empty("tbody")
  val Tr: Node.Empty[Int, MyAtt, Boolean] = Node.Empty("tr")
  val Td: Node.Empty[Boolean, MyAtt, String] = Node.Empty("td")

  val att = Attribute[MyAtt]("key", "value")

  val tab1: Node.Full[String, MyAtt, Symbol] = Table(Tbody(Tr(Td, Td(att))))
  val tab2 = Table(1, Tbody(att)(Tr, Tr(att), Tr(Td)))
  val tab3 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td("Hello world"))))


}
