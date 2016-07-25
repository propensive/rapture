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

import rapture.base._
import rapture.core._
import rapture.data._

import language.experimental.macros

private[css] object CssMacros {

  def parseSource(s: List[String], substitutions: List[String], stylesheet: Boolean): Option[(Int, Int, String)] =
    try {
      if(stylesheet) CssParser.parseStylesheet(s, substitutions)
      else CssParser.parse(s, substitutions)
      None
    } catch {
      case CssParser.ValidationException(strNo, pos, msg) =>
        Some((strNo, pos, s"failed to parse Css literal: $msg"))
    }

  // FIXME: Unify these three implementations, and use quasiquotes
  def cssClassContextMacro(c: BlackboxContext)(
      exprs: c.Expr[ForcedConversion[CssClass]]*): c.Expr[CssClass] = {
    import c.universe._
    import compatibility._

    c.prefix.tree match {
      case Select(Apply(_, List(Apply(_, rawPart :: Nil))), _) =>
        val Literal(Constant(className: String)) = rawPart
        if(!className.matches("-?[_a-zA-Z]+[_a-zA-Z0-9-]*"))
          c.abort(c.enclosingPosition, "this is not a valid CSS class identifier")

        c.Expr(q"_root_.rapture.css.CssClass(_root_.scala.collection.immutable.Set($rawPart))")
    }
  }
  def stylesheetContextMacro(c: BlackboxContext)(
      exprs: c.Expr[Embed[CssStylesheet]]*): c.Expr[CssStylesheet] = {
    import c.universe._
    import compatibility._

    c.prefix.tree match {
      case Select(Apply(_, List(Apply(_, rawParts))), _) =>
        val ys = rawParts
        val text = rawParts map { case lit @ Literal(Constant(part: String)) => part }

        val listExprs = c.Expr[List[Embed[CssStylesheet]]](
            Apply(
                Select(reify(List).tree, termName(c, "apply")),
                exprs.map(_.tree).to[List]
            ))

        val substitutions: List[String] = listExprs.tree match {
          case Apply(_, bs) =>
            bs.map {
              case Apply(Apply(TypeApply(Select(_, _), _), _), List(Select(_, nme))) => nme.toString match {
                case "domId" => "#foo"
                case "css" => "color: red;"
                case "cssClass" => ".foo"
                case "int" => "1"
                case "double" => "1.0"
                case _ => "null"
              }
            }
        }

        // FIXME: It looks like the cursor will be in the wrong place
        parseSource(text, substitutions, true) foreach {
          case (n, offset, msg) =>
            val oldPos = ys(n).asInstanceOf[Literal].pos
            val newPos = oldPos.withPoint(oldPos.startOrPoint + offset)
            c.error(newPos, msg)
        }

        val listParts = c.Expr[List[String]](
            Apply(
                Select(reify(List).tree, termName(c, "apply")),
                rawParts
            ))

        reify {
          val sb = new StringBuilder
          val textParts = listParts.splice.iterator
          val expressions: Iterator[Embed[CssStylesheet]] = listExprs.splice.iterator

          sb.append(textParts.next())

          while (textParts.hasNext) {
            sb.append(expressions.next.content)
            sb.append(textParts.next)
          }
          CssParser.parseStylesheet(List(sb.toString), Nil)
        }
    }
  }

  def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[Css]]*): c.Expr[Css] = {
    import c.universe._
    import compatibility._

    c.prefix.tree match {
      case Select(Apply(_, List(Apply(_, rawParts))), _) =>
        val ys = rawParts
        val text = rawParts map { case lit @ Literal(Constant(part: String)) => part }

        val listExprs = c.Expr[List[ForcedConversion[Css]]](
            Apply(
                Select(reify(List).tree, termName(c, "apply")),
                exprs.map(_.tree).to[List]
            ))

        val stringsUsed: List[String] = listExprs.tree match {
          case Apply(_, bs) =>
            bs.map {
              case Apply(Apply(TypeApply(Select(_, nme), _), _), _) => "null"
            }
        }

        parseSource(text, stringsUsed, false) foreach {
          case (n, offset, msg) =>
            val oldPos = ys(n).asInstanceOf[Literal].pos
            val newPos = oldPos.withPoint(oldPos.startOrPoint + offset)
            c.error(newPos, msg)
        }

        val listParts = c.Expr[List[String]](
            Apply(
                Select(reify(List).tree, termName(c, "apply")),
                rawParts
            ))

        reify {
          val sb = new StringBuilder
          val textParts = listParts.splice.iterator
          val expressions: Iterator[ForcedConversion[Css]] = listExprs.splice.iterator

          sb.append(textParts.next())

          while (textParts.hasNext) {
            sb.append(expressions.next.value.asInstanceOf[Css].content)
            sb.append(textParts.next)
          }
          CssParser.parse(List(sb.toString), Nil)
        }
    }
  }

}

private[css] class CssStrings(sc: StringContext) {
  class CssContext() {
    def apply(exprs: ForcedConversion[Css]*): Css = macro CssMacros.contextMacro
  }
  
  class CssClassContext() {
    def apply(exprs: Nothing*): CssClass = macro CssMacros.cssClassContextMacro
  }
  
  class StylesheetContext() {
    def apply(exprs: Embed[CssStylesheet]*): CssStylesheet = macro CssMacros.stylesheetContextMacro
  }
  val css = new CssContext()
  val cls = new CssClassContext()
  val cssStylesheet = new StylesheetContext()
}
