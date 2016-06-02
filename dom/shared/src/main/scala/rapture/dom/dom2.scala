package rapture.dom2

import rapture.core._

import language.dynamics
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

  /*def invalidAttribute[
    Att <: AttributeType: c.WeakTypeTag,
    Type <: AttributeType: c.WeakTypeTag
  ](c: BlackboxContext): c.Expr[ValidAttribute[Att, Type]] = {
    import c.universe.{Type => _, _}

    val expected = weakTypeOf[Att].toString//.split("\\.").last
    val found = weakTypeOf[Type].toString//.split("\\.").last

    c.abort(c.enclosingPosition, s"attempted to embed a $found node in a position where only $expected nodes are permitted")
  }*/

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

object `package` {
  
  implicit def convert[
    That <: NodeType,
    AttName <: String,
    This <: NodeType,
    ThatAtts <: AttributeType,
    Atts2 <: AttributeType,
    Child <: NodeType,
    Value
  ](x: (AttName, Value))(implicit embeddable: EmbeddableAttribute[x.type]): AttributeContent[That, ThatAtts, This, Child, Atts2] = ???
    //AttributeContent[That, ThatAtts, This, Child, Atts2](new Attribute(this.asInstanceOf[AttributeKey[AttributeType]], value))
  
  implicit def embedNodes[
    From,
    That <: NodeType,
    ThatAtts <: AttributeType,
    This <: NodeType,
    Atts <: AttributeType,
    Child <: NodeType
  ](from: From)(implicit embeddable: Embeddable[From, This, Atts, Child]): DomContent[That, ThatAtts, This, Child, Atts] =
    DomContent(embeddable.embed(from))
}

case class Attribute(name: String, value: String)

object EmbeddableAttribute {
  
  def apply[Value, Type]: WithType[Value, Type] = WithType[Value, Type]()
  
  case class WithType[Value, Type]() {
    def apply[S <: String](name: S): EmbeddableAttribute[(name.type, Value)] = new EmbeddableAttribute[(name.type, Value)](name)
  }
}

class EmbeddableAttribute[S <: (String, Value)](nameVal: S)

case class AttributeContent[
  That <: NodeType,
  ThatAtts <: AttributeType,
  This <: NodeType,
  Child <: NodeType,
  Atts <: AttributeType
](
  attribute: Attribute
) extends Content[That, ThatAtts, This, Child, Atts, Node.Attributed[That, ThatAtts, This]] {
 
  type Position = Attribute
  
  def value: Attribute = attribute
  
  def returnValue(
    empty: Node.Empty[That, ThatAtts, This],
    attributes: Iterable[Attribute]
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

/*abstract class AttributeKey[Name <: String, Atts <: AttributeType](val name: Name, actualName: String = null) {
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
}*/


object Node {

  object IsApply {
    implicit def isApplyImplicit = apply("apply")
    def apply[S <: String](s: S): IsApply[s.type] = null
  }
  trait IsApply[+S <: String]

  sealed trait TagStyle
  object Optional extends TagStyle
  object Required extends TagStyle
  object ClosingTagOptional extends TagStyle
  object EmptyTag extends TagStyle

  sealed trait DomNode[This <: NodeType, Atts <: AttributeType, Child <: NodeType] extends Product with Serializable

  sealed trait Element[This <: NodeType, Atts <: AttributeType, Child <: NodeType] extends DomNode[This, Atts, Child] {
    def tagStyle: TagStyle
    def name: String
    def attributes: Iterable[Attribute]
    def children: Iterable[DomNode[_, _, _]]
    
    override def toString = {
      val atts = if(attributes.isEmpty) "" else attributes.mkString(" ", " ", "")
	s"<$name$atts>${children.mkString}</$name>"
    }
  }

  object Empty {
    trait TypeOf { type Singleton }
    def typeOf[S <: String](s: S): TypeOf { type Singleton = s.type } = new TypeOf { type Singleton = s.type }
    val Apply = typeOf("apply")
    type Apply = Apply.type#Singleton
  }

  case class Empty[
    This <: NodeType,
    Atts <: AttributeType,
    Child <: NodeType
  ](
    tagStyle: TagStyle = Required
  )(
    implicit assignedName: AssignedName
  ) extends Element[This, Atts, Child] with Dynamic {
    
    def name = assignedName.name.toLowerCase
    
    def applyDynamic[Return](method: Empty.Apply)(head: Content[This, Atts, Child, _, _, Return], contents: Content[This, Atts, Child, _, _, Return]*): Return =
      head.returnValue(this, (head :: contents.to[List]).map(_.value.asInstanceOf[head.Position]))
    
    def applyDynamicNamed[Return](method: Empty.Apply)(head: Content[This, Atts, Child, _, _, Return], contents: Content[This, Atts, Child, _, _, Return]*): Return =
      head.returnValue(this, (head :: contents.to[List]).map(_.value.asInstanceOf[head.Position]))
    
    def children = Iterable[DomNode[_, _, _]]()
    def attributes = Iterable()

    override def toString = s"<$name/>"
  }
  
  case class Attributed[This <: NodeType, Atts <: AttributeType, Child <: NodeType](
    tagStyle: TagStyle,
    name: String,
    attributes: Iterable[Attribute]
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
    attributes: Iterable[Attribute],
    children: Iterable[DomNode[_, _, _]]
  ) extends Element[This, Atts, Child]

  case class TextNode[Type <: NodeType, Atts <: AttributeType, Child <: NodeType](content: String) extends DomNode[Type, Atts, Child] {
    override def toString = content
  }

}
