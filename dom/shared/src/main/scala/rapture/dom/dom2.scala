package rapture.dom2

import rapture.core._

import language.implicitConversions
import language.higherKinds
import language.existentials
import language.experimental.macros

object DomMacros {
  import rapture.base._

  def fallbackEmbeddable[
    Type <: NodeType: c.WeakTypeTag,
    Atts <: AttributeType,
    Child <: NodeType,
    Type2 <: NodeType: c.WeakTypeTag
  ](c: BlackboxContext): c.Expr[Embeddable[Node.DomNode[Type, Atts, Child], Type2, Atts, Child]] = {
    import c.universe.{Type => _, _}

    val expected = weakTypeOf[Type2].toString.split("\\.").last
    val found = weakTypeOf[Type].toString.split("\\.").last

    c.abort(c.enclosingPosition, s"attempted to embed a $found node in a position where only $expected nodes are permitted")
  }

}

  trait One extends NodeType
  trait Two extends NodeType
  trait Three extends NodeType
  trait Four extends NodeType
  trait Five extends NodeType

  trait NodeType
  trait AttributeType

  trait Content[
    That <: NodeType,
    ThatAtts <: AttributeType,
    This <: NodeType,
    Child <: NodeType,
    Atts <: AttributeType,
    Return
  ] {
    
    type Position
    
    def value: Position

    def returnValue(empty: Node.Empty[That, ThatAtts, This], elements: Seq[Position]): Return
  }

  object `package` {
    implicit def embedNodes[
      From,
      That <: NodeType,
      ThatAtts <: AttributeType,
      This <: NodeType,
      Atts <: AttributeType,
      Child <: NodeType
    ](from: From)(implicit embeddable: Embeddable[From, This, Atts, Child]): DomContent[That, ThatAtts, This, Child, Atts] =
      DomContent(embeddable.embed(from))
    
    implicit def embedAttributes[
      That <: NodeType,
      ThatAtts <: AttributeType,
      This <: NodeType,
      Child <: NodeType,
      Atts <: AttributeType
    ](from: Attribute[ThatAtts]): AttributeContent[That, ThatAtts, This, Child, Atts] =
      AttributeContent(from)
  }
  
  case class AttributeContent[That <: NodeType, ThatAtts <: AttributeType, This <: NodeType, Child <: NodeType, Atts <: AttributeType](
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

  case class DomContent[
    That <: NodeType,
    ThatAtts <: AttributeType,
    This <: NodeType,
    Child <: NodeType,
    Atts <: AttributeType
  ](
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
  

  object Embeddable extends Embeddable_1 {

    implicit def domNodeEmbeddable[
      Type <: NodeType,
      Atts <: AttributeType,
      Child <: NodeType,
      Type2 <: Type
    ](implicit ev: Type <:< Type2): Embeddable[Node.DomNode[Type, Atts, Child], Type2, Atts, Child] =
      new Embeddable[Node.DomNode[Type, Atts, Child], Type2, Atts, Child] {
        def embed(from: Node.DomNode[Type, Atts, Child]) =
	  Seq(from.asInstanceOf[Node.DomNode[Type2, Atts, Child]])
      }
  
    implicit def stringEmbeddable[
      Type <: NodeType,
      Atts <: AttributeType,
      Child <: NodeType,
      From: StringSerializer
    ]: Embeddable[From, Type, Atts, Child] =
      new Embeddable[From, Type, Atts, Child] {
        def embed(from: From) =
	  Seq(Node.Text(implicitly[StringSerializer[From]].serialize(from)))
      }
  }

  trait Embeddable_1 {
    implicit def fallbackEmbeddable[
      Type <: NodeType,
      Atts <: AttributeType,
      Child <: NodeType,
      Type2 <: NodeType
    ]: Embeddable[Node.DomNode[Type, Atts, Child], Type2, Atts, Child] =
      macro DomMacros.fallbackEmbeddable[Type, Atts, Child, Type2]
  }

  trait Embeddable[-From, Type <: NodeType, Atts <: AttributeType, Child <: NodeType] {
    def embed(from: From): Seq[Node.DomNode[Type, Atts, Child]]
  }

  case class Attribute[Att](key: String, value: String) {
    override def toString = s"""$key="$value""""
  }

  object Node {
    
    sealed trait DomNode[This <: NodeType, Atts <: AttributeType, Child <: NodeType] extends Product with Serializable

    sealed trait Element[This <: NodeType, Atts <: AttributeType, Child <: NodeType] extends DomNode[This, Atts, Child] {
      def name: String
      def attributes: Seq[Attribute[Atts]]
      def children: Seq[DomNode[Child, _, _]]
      
      override def toString = {
        val atts = if(attributes.isEmpty) "" else attributes.mkString(" ", " ", "")
	s"<$name$atts>${children.mkString}</$name>"
      }
    }

    case class Empty[This <: NodeType, Atts <: AttributeType, Child <: NodeType](name: String) extends Element[This, Atts, Child] {
      
      def apply[Return](contents: Content[This, Atts, Child, _, _, Return]*): Return = {
	val head = contents.head
	head.returnValue(this, contents.map(_.value.asInstanceOf[head.Position]))
      }
      
      def children = Seq[DomNode[Child, _, _]]()
      def attributes = Seq()

      override def toString = s"<$name/>"
    }
    
    case class Attributed[This <: NodeType, Atts <: AttributeType, Child <: NodeType](
      name: String,
      attributes: Seq[Attribute[Atts]]
    ) extends Element[This, Atts, Child] {
      
      def children = Seq[DomNode[Child, _, _]]()
      
      def apply[Grandchild <: NodeType, ChildAtts <: AttributeType](
        content: DomContent[This, Atts, Child, Grandchild, ChildAtts]*
      ): Full[This, Atts, Child] =
        Full(name, attributes, content.flatMap(_.nodes))
    }

    case class Full[This <: NodeType, Atts <: AttributeType, Child <: NodeType](
      name: String,
      attributes: Seq[Attribute[Atts]],
      children: Seq[DomNode[Child, _, _]]
    ) extends Element[This, Atts, Child]

    case class Text[Type <: NodeType, Atts <: AttributeType, Child <: NodeType](content: String) extends DomNode[Type, Atts, Child] {
      override def toString = content
    }

  }

object Dom2Test {

  trait MyAtt extends AttributeType
  val Table: Node.Empty[One, MyAtt, Two] = Node.Empty("table")
  val Tbody: Node.Empty[Two, MyAtt, Three] = Node.Empty("tbody")
  val Tr: Node.Empty[Three, MyAtt, Four] = Node.Empty("tr")
  val Td: Node.Empty[Four, MyAtt, Five] = Node.Empty("td")

  val att = Attribute[MyAtt]("key", "value")

  val tab1: Node.Full[One, MyAtt, Two] = Table(Tbody(Tr(Td, Td(att))))
  val tab2 = Table(1, Tbody(att)(Tr, Tr(att), Tr(Td)))
  val tab3 = Table(att)(Tbody(att)(Tr, Tr(att), Tr(Td("Hello world"))))


}
