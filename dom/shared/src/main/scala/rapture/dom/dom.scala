/*
  Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.

  The primary distribution site is
  
    http://rapture.io/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  
    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is
  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and limitations under the License.
*/

package rapture.dom

import rapture.core._

import language.higherKinds
import language.implicitConversions
import language.dynamics

import scala.annotation._

import language.experimental.macros

trait ElementType
trait AttributeType

trait `DomNodes#apply` extends MethodConstraint
trait `Element#selectDynamic` extends MethodConstraint

object Applicable {
  implicit def reportElementError[Child <: ElementType, Att <: AttributeType, Child2 <: ElementType, Elem <: ElementType,
      Att2 <: AttributeType](value: DomNode[Child2, Elem, Att]): Any =
    macro DomMacros.reportElementErrorMacro[Child, Att, Child2, Elem, Att2]

  implicit def listToApplicable[Child <: ElementType, Elem <: ElementType, Att <: AttributeType, Ap <: Element[Child, Elem, Att]](apps: List[Ap]): Element[Child, Elem, Att] = apps.head

  implicit def wrapIterable[Child <: ElementType, This <: ElementType, Att <: AttributeType](xs:
      Iterable[Element[Child, This, Att]]): ElementLike[Child, This, Att] = ElementSeq[Child, This, Att](xs)

}

trait Applicable[+ChildType, +ThisAttType, AppliedType[_ <: ElementType, _ <: ElementType,
    _ <: AttributeType]] {
  def application[Child <: ElementType, This <: ElementType, Att <: AttributeType]
      (element: Element[Child, This, Att], applied: Applicable[Child, Att, AppliedType]*):
      AppliedType[Child, This, Att]
}

sealed abstract class DomNode[+ChildType <: ElementType, +ThisType <: ElementType, ThisAttType <: AttributeType]
    extends Applicable[ThisType, Nothing, AppliedElement] {
 
  def block = true

  def application[Child <: ElementType, This <: ElementType, Att <: AttributeType]
      (element: Element[Child, This, Att], applied: Applicable[Child, Att, AppliedElement]*):
      AppliedElement[Child, This, Att] =
    AppliedElement[Child, This, Att](
      element.tagName,
      element.attributes,
      applied.to[List].asInstanceOf[List[Element[_ <: ElementType, Child, _ <: AttributeType]]],
      element.forceClosingTag,
      element.block
    )
}

case class DomNodes[ChildType <: ElementType, ThisType <: ElementType, AttType <: AttributeType]
    (elements: Vector[Element[ChildType, ThisType, AttType]]) extends AnyVal {
  
  def apply(i: Int = 0)(implicit mode: Mode[`DomNodes#apply`]) = mode.wrap(elements(i))
  
  override def toString = elements.mkString("\n")
  
  def \[Child <: ElementType, This <: ElementType, Att <: AttributeType](tag: Tag[Child, This, Att]):
      DomNodes[Child, This, Att] = DomNodes(elements.flatMap { xs => (xs \ tag).elements })

  def \\[Child <: ElementType, This <: ElementType, Att <: AttributeType](tag: Tag[Child, This, Att]):
      DomNodes[Child, This, Att] = DomNodes(elements.flatMap { xs => (xs \\ tag).elements })
}

case class MissingAttributeException(name: String) extends Exception(s"The attribute $name was not found")

case class TextNode[ThisType <: ElementType, ThisAttType <: AttributeType, Position <: ElementType]
    (text: String) extends DomNode[Position, ThisType, ThisAttType] {
  def format[Output](implicit formatter: DomFormatter[Output]): Output = formatter.format(this)
  override def toString = DomFormatter.domFormatterImplicit.format(this)
  override def block = false
}

case class Comment(comment: String) extends DomNode {
  def format[Output](implicit formatter: DomFormatter[Output]): Output = formatter.format(this)
  override def toString = DomFormatter.domFormatterImplicit.format(this)
}

object Element {
  implicit def reportElementError2[Child <: ElementType, Att <: AttributeType, Child2 <: ElementType, Elem <: ElementType]
      (value: DomNode[Child2, Elem, Att]): Any =
    macro DomMacros.reportElementError2Macro[Child, Att, Child2, Elem]
}

sealed abstract class ElementLike[ChildType <: ElementType, ThisType <: ElementType, AttType <: AttributeType]
    extends DomNode[ChildType, ThisType, AttType] with Applicable[ThisType, Nothing, AppliedElement]
    with Dynamic with Product with Serializable {

}

sealed abstract class Element[ChildType <: ElementType, ThisType <: ElementType, AttType <: AttributeType]
    extends ElementLike[ChildType, ThisType, AttType] {
  
  def tagName: String
  def attributes: Map[AttributeKey[String, AttributeType], Any]
  def children: List[DomNode[_ <: ElementType, ChildType, _ <: AttributeType]]
  def forceClosingTag: Boolean
  def block: Boolean

  def format[Output](implicit formatter: DomFormatter[Output]): Output = formatter.format(this)
 
  def selectDynamic(s: String)(implicit ar: AttributeKey[s.type, AttType], mode: Mode[`Element#selectDynamic`]) =
    mode.wrap {
      val ar2 = ar.asInstanceOf[AttributeKey[String, AttributeType]]
      if(attributes.contains(ar2)) attributes(ar2).asInstanceOf[ar.Value]
      else mode.exception(MissingAttributeException(ar2.name))
    }

  def \[Child <: ElementType, This <: ElementType, Att <: AttributeType](tag: Tag[Child, This, Att]):
      DomNodes[Child, This, Att] =
    DomNodes[Child, This, Att](children.filter {
      case elem: Element[_, _, _] => elem.tagName == tag.tagName
      case _ => false
    }.map(_.asInstanceOf[Element[Child, This, Att]]).to[Vector])

  def \\[Child <: ElementType, This <: ElementType, Att <: AttributeType](tag: Tag[Child, This, Att]):
      DomNodes[Child, This, Att] =
    DomNodes(children.to[Vector].flatMap {
      case e: Element[_, _, _] =>
        val found = e \ tag
        found.elements ++ (e \\ tag).elements
      case _ => Vector()
    })

  override def toString = DomFormatter.domFormatterImplicit.format(this)

}

case class Tag[ChildType <: ElementType, ThisType <: ElementType, AttType <: AttributeType]
    (forceClosingTag: Boolean = false, override val block: Boolean = true)(implicit assigned: AssignedName) extends
    Element[ChildType, ThisType, AttType] {

  type Content = AppliedElement[_ <: ElementType, ChildType, _ <: AttType]

  def tagName = assigned.name.toLowerCase
  def attributes = Map()
  def children = List()

  def apply[AppliedType[_ <: ElementType, _ <: ElementType, _ <: AttributeType] <: DomNode[_, _, _]](
      first: Applicable[ChildType, AttType, AppliedType], applied: Applicable[ChildType, AttType, AppliedType]*):
      AppliedType[ChildType, ThisType, AttType] = first.application(this, first +: applied: _*)
}

case class ElementSeq[ChildType <: ElementType, ThisType <: ElementType, AttType <: AttributeType](elems:
    Iterable[Element[ChildType, ThisType, AttType]]) extends ElementLike[ChildType, ThisType, AttType]

case class EmptyElement[ChildType <: ElementType, ThisType <: ElementType, AttType <: AttributeType](
  tagName: String,
  attributes: Map[AttributeKey[String, AttributeType], Any],
  forceClosingTag: Boolean = false,
  override val block: Boolean = true
) extends Element[ChildType, ThisType, AttType] {

  def children = List()
  
  def apply(elements: DomNode[_ <: ElementType, ChildType, _ <: AttributeType]*): AppliedElement[ChildType, ThisType, AttType] =
    AppliedElement[ChildType, ThisType, AttType](tagName, attributes, elements.to[List], forceClosingTag, block)
}

case class AppliedElement[ChildType <: ElementType, ThisType <: ElementType, AttType <: AttributeType](
  tagName: String,
  attributes: Map[AttributeKey[String, AttributeType], Any] = Map(),
  children: List[DomNode[_ <: ElementType, ChildType, _ <: AttributeType]] = Nil,
  forceClosingTag: Boolean,
  override val block: Boolean
) extends Element[ChildType, ThisType, AttType]


@implicitNotFound("Cannot access the attribute ${Name} on ${AttType} DOM nodes")
abstract class AttributeKey[+Name <: String, AttType <: AttributeType](val name: String, actualName: String = null) {
  type Value
  override def toString = if(actualName == null) name else actualName
  def serialize(t: Value): String

  def set[Elem <: ElementType](v: Value) = new Attribute[Elem, AttType, Value](this.asInstanceOf[AttributeKey[String, AttributeType]], v)

  override def hashCode = name.hashCode
  override def equals(that: Any) = that match {
    case ar: AttributeKey[typ, attType] => ar.name == name
    case _ => false
  }
}

object Attribute {
  def apply[Att <: AttributeType, V](name: String, actualName: String = null)(serializer: V => String):
      AttributeKey[name.type, Att] { type Value = V } =
    new AttributeKey[name.type, Att](if(actualName == null) name else actualName) {
      type Value = V
      def serialize(v: Value): String = serializer(v)
    }
}

class Attribute[Elem <: ElementType, AttType <: AttributeType, Value]
    (val id: AttributeKey[String, AttributeType], val value: Value) extends
    Applicable[Elem, AttType, EmptyElement] {
  
  def name = id.name
  
  def application[Child <: ElementType, This <: ElementType, Att <: AttributeType]
      (element: Element[Child, This, Att], applied: Applicable[Child, Att, EmptyElement]*):
      EmptyElement[Child, This, Att] = {

    val as = applied.to[List].map { x =>
      val y = x.asInstanceOf[Attribute[ElementType, AttributeType, Any]]
      y.id -> y.value.asInstanceOf[Any]
    }.toMap

    EmptyElement(element.tagName, as, element.forceClosingTag, element.block)
  }
}
