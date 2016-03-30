package rapture.dom2

import language.implicitConversions
import language.higherKinds

object Dom2 {

  sealed trait ContentType { type Return[_, _] }
  case class AttributeContent(key: String, value: String) extends ContentType { type Return[T, C] = Tag.Attributed[T, C] }
  case class TagContent[T](node: Any) extends ContentType { type Return[T, C] = Tag.Full[T, C] }

  trait IsContent[-T, +Out <: ContentType]

  implicit def use[E, Out <: ContentType, Return, C](e: E)(implicit ev: IsContent[E, Out]): Content[Out, Out, Return, C] = ???
 
  implicit def stringIsContent[T]: IsContent[String, TagContent[T]] = ???

  object Content
  
  case class Content[+ElemType <: ContentType, +Applicable <: ContentType, -Return, +Children]()

  object Tag {
    implicit def tagIsContent[T, C]: IsContent[Tag[_, C], TagContent[T]] = ???
    implicit def tagIsContent2[T, C]: IsContent[List[Tag[_, C]], TagContent[T]] = ???
  
    object Empty

    case class Empty[This, Children]() extends Tag[This, Children](Nil, Nil) {
      def apply[T <: ContentType, Return, C](elems: Content[T, ContentType, Return, C]*): T#Return[_, C] = ???
    }
  
    object Attributed
  
    case class Attributed[This, Children](override val attributes: Seq[Attribute]) extends Tag[This, Children](attributes, Nil) {
      def apply[T <: ContentType, Return, C](elements: Content[T, TagContent[_], Return, C]*): T#Return[_, C] = ???
    }

    object Full

    case class Full[This, Children](override val attributes: Seq[Attribute], override val children: Seq[Tag[_, _]]) extends Tag[This, Children](attributes, children) {
  
    }

  }
  
  class Tag[This, Children](val attributes: Seq[Attribute], val children: Seq[Tag[_, _]])

  object Attribute {
    implicit def attributeContent: IsContent[Attribute, AttributeContent] = ???
  }
  
  class Attribute(val key: String, value: String)

  val att = new Attribute("key", "value")

  object Table extends Tag.Empty[One, Two]()
  object Tbody extends Tag.Empty[Two, Three]()
  object Tr extends Tag.Empty[Three, Four]()
  object Td extends Tag.Empty[Four, Five]()
  object Th extends Tag.Empty[Four, Five]()

}


trait One
trait Two extends One
trait Three extends Two
trait Four extends Three
trait Five extends Four

object DomTest {
  import Dom2._

  Tbody(att)
  Tbody(use(List(Tr, Tr, Tr(att))))
  Tbody(att)(Tr, Td)
  
}

