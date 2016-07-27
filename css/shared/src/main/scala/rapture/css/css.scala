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

package rapture.css

import rapture.dom._
import rapture.core._

import scala.collection.immutable.ListMap

object Css {

  implicit def stringSerializer: StringSerializer[Css] = new StringSerializer[Css] {
    def serialize(css: Css): String = css.content
  }
}

case class Css(properties: ListMap[String, String]) {
  def content = properties.map { case (k, v) => s"$k: $v;" }.mkString(" ")
  override def toString = s"""css${"\"" * 3}$content${"\"" * 3}"""
  def +(css: Css) = Css(properties ++ css.properties)
}

object CssStylesheet { 
  implicit def stringSerializer: StringSerializer[CssStylesheet] = new StringSerializer[CssStylesheet] {
    def serialize(css: CssStylesheet): String = css.content
  }
}
case class CssStylesheet(rules: List[CssRule]) {
  override def toString = s"""cssStylesheet${"\"" * 3}$content${"\"" * 3}"""
  def content = rules.mkString("\n")
}

object DomId {
  def auto(implicit assignedName: AssignedName) = DomId(assignedName.name)
}
case class DomId(id: String)

object CssClass {
  def auto(implicit assignedName: AssignedName) = CssClass(Set(assignedName.name))
  val empty = CssClass(Set())
}

case class CssClass(classes: Set[String]) {
  def +(cssClass: CssClass): CssClass = CssClass(classes ++ cssClass.classes)

  def asString = classes.mkString(" ")
}

object CssEmbed {
  implicit def embedCssClass(cssClass: CssClass): CssEmbed = CssEmbed(cssClass.classes.mkString(".", ".", ""))
  implicit def embedDomId(domId: DomId): CssEmbed = CssEmbed(s"#${domId.id}")
}
case class CssEmbed(content: String)


object Embeddable {
  implicit val domId: Embeddable[DomId, CssStylesheet] = new Embeddable[DomId, CssStylesheet] { def embed(value: DomId): String = s"#${value.id}" }
  
  implicit val cssClass: Embeddable[CssClass, CssStylesheet] = new Embeddable[CssClass, CssStylesheet] {
    def embed(value: CssClass): String = value.classes.mkString(".", ".", "")
  }
  
  implicit val domTag: Embeddable[Tag[_, _, _], CssStylesheet] = new Embeddable[Tag[_, _, _], CssStylesheet] {
    def embed(value: Tag[_, _, _]): String = value.tagName.toLowerCase
  }
  
  implicit val css: Embeddable[Css, CssStylesheet] = new Embeddable[Css, CssStylesheet] {
    def embed(value: Css): String = value.content
  }
  
  implicit val string: Embeddable[String, CssStylesheet] = new Embeddable[String, CssStylesheet] {
    def embed(value: String): String = value
  }
  
  implicit val int: Embeddable[Int, CssStylesheet] = new Embeddable[Int, CssStylesheet] {
    def embed(value: Int): String = value.toString
  }
  
  implicit val double: Embeddable[Double, CssStylesheet] = new Embeddable[Double, CssStylesheet] {
    def embed(value: Double): String = value.toString
  }
}

trait Embeddable[-From, +To] { def embed(value: From): String }

object Embed {
  implicit def embed[From, To](value: From)(implicit embeddable: Embeddable[From, To]): Embed[To] = Embed(embeddable.embed(value))
}

case class Embed[To](content: String)
