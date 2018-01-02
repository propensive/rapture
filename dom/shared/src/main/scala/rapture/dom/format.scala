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

package domFormatters {
  object compact {
    implicit val domFormatterImplicit = new DomFormatter[String] {
      def format(element: DomNode[_, _, _]): String = element match {
        case TextNode(t) =>
          text(t)

        case Comment(c) =>
          comment(c)

        case elems: ElementSeq[_, _, _] =>
          elems.elems.map { elem =>
            val xs = elem.children.map(format).mkString

            val as = elem.tagName +: elem.attributes.to[List].filter(_._2 != null).map {
              case (k, v) =>
                k.attributeName + "=\"" + k.serialize(v.asInstanceOf[k.Value]) + "\""
            }

            if (xs.isEmpty && !elem.forceClosingTag) s"<${as.mkString(" ")}/>"
            else s"<${as.mkString(" ")}>$xs</${elem.tagName}>"
          }.mkString

        case elem: Element[_, _, _] =>
          val xs = elem.children.map(format).mkString

          val as = elem.tagName +: elem.attributes.to[List].filter(_._2 != null).map {
            case (k, v) =>
              k.attributeName + "=\"" + k.serialize(v.asInstanceOf[k.Value]) + "\""
          }

          if (xs.isEmpty && !elem.forceClosingTag) s"<${as.mkString(" ")}/>"
          else s"<${as.mkString(" ")}>$xs</${elem.tagName}>"
      }
    }
  }
}

object DomFormatter {
  implicit val domFormatterImplicit: DomFormatter[String] = new DomFormatter[String] {

    def format(element: DomNode[_, _, _]): String =
      format(0, element).map {
        case (i, s) =>
          ("  " * i) + s
      }.mkString("\n")

    protected def format(indent: Int, element: DomNode[_, _, _]): Vector[(Int, String)] = element match {
      case TextNode(t) =>
        Vector(indent -> text(t))

      case Comment(c) =>
        Vector(indent -> comment(c))

      case elem: Element[_, _, _] =>
        val children = elem.children.to[Vector]
        //.asInstanceOf[Vector[DomNode[ElementType, ElementType, AttributeType]]]
        val xs = children
          .foldLeft(Vector[(Int, String)]() -> false) {
            case ((Vector(), _), child) =>
              (format(indent + 1, child), child.block)
            case ((acc, blk), child) =>
              val cs = format(indent + 1, child)
              val join = cs.length == 1 && !blk
              if (join) (acc.init :+ ((acc.last._1, acc.last._2 + cs(0)._2)), !join)
              else (acc ++ cs, !join)
          }
          ._1

        val hasBlock = elem.children.exists(_.block)

        val as = elem.tagName +: elem.attributes.to[List].filter(_._2 != null).map {
          case (k, v) =>
            k.attributeName + "=\"" + k.serialize(v.asInstanceOf[k.Value]) + "\""
        }

        if (xs.isEmpty && !elem.forceClosingTag) Vector(indent -> s"<${as.mkString(" ")}/>")
        else if (hasBlock || elem.block)
          (indent -> s"<${as.mkString(" ")}>") +: xs :+ (indent -> s"</${elem.tagName}>")
        else Vector(indent -> s"<${as.mkString(" ")}>${xs.map(_._2).mkString}</${elem.tagName}>")

      case elem: ElementSeq[_, _, _] =>
        elem.elems.to[Vector].flatMap(format(indent, _))
    }
  }
}

trait DomFormatter[Output] {
  protected def text(string: String): String = string.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
  protected def comment(string: String) = "<!--" + string.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;") + "-->"

  def format(element: DomNode[_, _, _]): Output
}
