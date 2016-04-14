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

  def invalidAttribute[
    Att <: AttributeType: c.WeakTypeTag,
    Type <: AttributeType: c.WeakTypeTag
  ](c: BlackboxContext): c.Expr[ValidAttribute[Att, Type]] = {
    import c.universe.{Type => _, _}

    val expected = weakTypeOf[Att].toString//.split("\\.").last
    val found = weakTypeOf[Type].toString//.split("\\.").last

    c.abort(c.enclosingPosition, s"attempted to embed a $found node in a position where only $expected nodes are permitted")
  }

}

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

  def returnValue(empty: Node.Empty[That, ThatAtts, This], elements: Iterable[Position]): Return
}

object `package` extends package_1 {
  implicit def embedNodes[
    From,
    That <: NodeType,
    ThatAtts <: AttributeType,
    This <: NodeType,
    Atts <: AttributeType,
    Child <: NodeType
  ](from: From)(implicit embeddable: Embeddable[From, This, Atts, Child]): DomContent[That, ThatAtts, This, Child, Atts] =
    DomContent(embeddable.embed(from))

  implicit def validAttribute[Att <: AttributeType, Type <: Att]: ValidAttribute[Att, Type] = null
}
  
trait package_1 {
  implicit def invalidAttribute[Att <: AttributeType, Type <: AttributeType]: ValidAttribute[Att, Type] =
    macro DomMacros.invalidAttribute[Att, Type]
}

trait ValidAttribute[Att, Type]


case class AttributeContent[That <: NodeType, ThatAtts <: AttributeType, This <: NodeType, Child <: NodeType, Atts <: AttributeType](
  attribute: Attribute[_ <: NodeType, ThatAtts, _]
) extends Content[That, ThatAtts, This, Child, Atts, Node.Attributed[That, ThatAtts, This]] {
 
  type Position = Attribute[_ <: NodeType, ThatAtts, _]
  
  def value: Attribute[_ <: NodeType, ThatAtts, _] = attribute
  
  def returnValue(
    empty: Node.Empty[That, ThatAtts, This],
    attributes: Iterable[Attribute[_ <: NodeType, ThatAtts, _]]
  ): Node.Attributed[That, ThatAtts, This] =
    Node.Attributed(empty.tagStyle, empty.name, attributes)
}

case class DomContent[
  That <: NodeType,
  ThatAtts <: AttributeType,
  This <: NodeType,
  Child <: NodeType,
  Atts <: AttributeType
](
  nodes: Iterable[Node.DomNode[_, _, _]]
) extends Content[That, ThatAtts, This, Child, Atts, Node.Full[That, ThatAtts, This]] {
  
  type Position = Iterable[Node.DomNode[_, _, _]]

  def value: Iterable[Node.DomNode[_, _, _]] = nodes

  def returnValue(
    empty: Node.Empty[That, ThatAtts, This],
    nodes: Iterable[Iterable[Node.DomNode[_, _, _]]]
  ): Node.Full[That, ThatAtts, This] =
    Node.Full(empty.tagStyle, empty.name, Nil, nodes.flatten)
}


object Embeddable extends Embeddable_1 {

  implicit def iterableDomNodeEmbeddable[
    Type <: NodeType,
    Atts <: AttributeType,
    Child <: NodeType,
    Type2 <: NodeType
  ](implicit ev: Type <:< Type2): Embeddable[Iterable[Node.DomNode[Type, Atts, Child]], Type2, Atts, Child] =
    new Embeddable[Iterable[Node.DomNode[Type, Atts, Child]], Type2, Atts, Child] {
      def embed(from: Iterable[Node.DomNode[Type, Atts, Child]]) =
	  from.asInstanceOf[Iterable[Node.DomNode[_, _, _]]]
    }

  implicit def domNodeEmbeddable[
    Type <: NodeType,
    Atts <: AttributeType,
    Child <: NodeType,
    Type2 <: NodeType
  ](implicit ev: Type <:< Type2): Embeddable[Node.DomNode[Type, Atts, Child], Type2, Atts, Child] =
    new Embeddable[Node.DomNode[Type, Atts, Child], Type2, Atts, Child] {
      def embed(from: Node.DomNode[Type, Atts, Child]) =
	  Iterable(from.asInstanceOf[Node.DomNode[Type2, Atts, Child]])
    }

  implicit def stringEmbeddable[
    Type <: NodeType,
    Atts <: AttributeType,
    Child <: NodeType,
    From: StringSerializer
  ]: Embeddable[From, Type, Atts, Child] =
    new Embeddable[From, Type, Atts, Child] {
      def embed(from: From) =
	  Iterable(Node.TextNode(implicitly[StringSerializer[From]].serialize(from)))
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
  def embed(from: From): Iterable[Node.DomNode[_, _, _]]
}

abstract class AttributeKey[Atts <: AttributeType](val name: String, actualName: String = null) {
  type Value
  override def toString = if(actualName == null) name else actualName
  def serialize(t: Value): String

  def set[
    That <: NodeType,
    ThatAtts <: AttributeType,
    This <: NodeType,
    Child <: NodeType,
    Atts2 <: AttributeType
  ](value: Value)(): AttributeContent[That, ThatAtts, This, Child, Atts2] =
    AttributeContent[That, ThatAtts, This, Child, Atts2](new Attribute(this.asInstanceOf[AttributeKey[AttributeType]], value))
  
  override def hashCode = name.hashCode
  override def equals(that: Any) = that match {
    case ar: AttributeKey[attType] => ar.name == name
    case _ => false
  }
}


object Attribute {
  def apply[Att <: AttributeType, V](name: String, actualName: String = null)(serializer: V => String):
      AttributeKey[Att] { type Value = V } =
    new AttributeKey[Att](if(actualName == null) name else actualName) {
      type Value = V
      def serialize(v: Value): String = serializer(v)
    }
}

case class Attribute[Elem <: NodeType, Atts <: AttributeType, Value](
  id: AttributeKey[AttributeType],
  value: Value
) {

  def name = id.name

  override def toString = s"""${id.name}="${id.serialize(value.asInstanceOf[id.Value])}""""
}


object Node {
  
  sealed trait TagStyle
  object Optional extends TagStyle
  object Required extends TagStyle
  object ClosingTagOptional extends TagStyle
  object EmptyTag extends TagStyle

  sealed trait DomNode[This <: NodeType, Atts <: AttributeType, Child <: NodeType] extends Product with Serializable

  sealed trait Element[This <: NodeType, Atts <: AttributeType, Child <: NodeType] extends DomNode[This, Atts, Child] {
    def tagStyle: TagStyle
    def name: String
    def attributes: Iterable[Attribute[_, Atts, _]]
    def children: Iterable[DomNode[_, _, _]]
    
    override def toString = {
      val atts = if(attributes.isEmpty) "" else attributes.mkString(" ", " ", "")
	s"<$name$atts>${children.mkString}</$name>"
    }
  }

  case class Empty[
    This <: NodeType,
    Atts <: AttributeType,
    Child <: NodeType
  ](
    tagStyle: TagStyle = Required
  )(
    implicit assignedName: AssignedName
  ) extends Element[This, Atts, Child] {
    
    def name = assignedName.name.toLowerCase
    
    def apply[Return](head: Content[This, Atts, Child, _, _, Return], contents: Content[This, Atts, Child, _, _, Return]*): Return =
      head.returnValue(this, (head :: contents.to[List]).map(_.value.asInstanceOf[head.Position]))
    
    def children = Iterable[DomNode[_, _, _]]()
    def attributes = Iterable()

    override def toString = s"<$name/>"
  }
  
  case class Attributed[This <: NodeType, Atts <: AttributeType, Child <: NodeType](
    tagStyle: TagStyle,
    name: String,
    attributes: Iterable[Attribute[_ <: NodeType, Atts, _]]
  ) extends Element[This, Atts, Child] {
    
    def children = Iterable[DomNode[_, _, _]]()
    
    def apply[Grandchild <: NodeType, ChildAtts <: AttributeType](
      head: DomContent[This, Atts, Child, Grandchild, ChildAtts],
      content: DomContent[This, Atts, Child, Grandchild, ChildAtts]*
    ): Full[This, Atts, Child] =
      Full(tagStyle, name, attributes, (head :: content.to[List]).flatMap(_.nodes))
  }

  case class Full[This <: NodeType, Atts <: AttributeType, Child <: NodeType](
    tagStyle: TagStyle,
    name: String,
    attributes: Iterable[Attribute[_ <: NodeType, Atts, _]],
    children: Iterable[DomNode[_, _, _]]
  ) extends Element[This, Atts, Child]

  case class TextNode[Type <: NodeType, Atts <: AttributeType, Child <: NodeType](content: String) extends DomNode[Type, Atts, Child] {
    override def toString = content
  }

}
