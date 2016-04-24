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

package rapture.js

import rapture.base._
import rapture.core._
import rapture.data._

import language.experimental.macros

private[js] object JsMacros {
  
  def parseSource(s: List[String], stringsUsed: List[Boolean]): Option[(Int, Int, String)] = try {
    JsValidator.validate(s)
    None
  } catch {
    case JsValidator.ValidationException(strNo, pos, msg) =>
      Some((strNo, pos, s"failed to parse Js literal: $msg"))
  }
  
  def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[Js]]*): c.Expr[Js] = {
    import c.universe._
    import compatibility._

    c.prefix.tree match {
      case Select(Apply(_, List(Apply(_, rawParts))), _) =>
        val ys = rawParts.to[List]
	val text = rawParts map { case lit@Literal(Constant(part:  String)) => part }

	val listExprs = c.Expr[List[ForcedConversion[Js]]](Apply(
	  Select(reify(List).tree, termName(c, "apply")),
	  exprs.map(_.tree).to[List]
	))

        val stringsUsed: List[Boolean] = listExprs.tree match {
          case Apply(_, bs) => bs.map {
            case Apply(Apply(TypeApply(Select(_, nme), _), _), _) => nme.toString == "forceStringConversion"
	  }
	}

	parseSource(text, stringsUsed) foreach { case (n, offset, msg) =>
	  val oldPos = ys(n).asInstanceOf[Literal].pos
	  val newPos = oldPos.withPoint(oldPos.startOrPoint + offset)
	  c.error(newPos, msg)
	}

	val listParts = c.Expr[List[String]](Apply(
	  Select(reify(List).tree, termName(c, "apply")),
	  rawParts
	))

	reify {
          val sb = new StringBuilder
	  val textParts = listParts.splice.iterator
          val expressions: Iterator[ForcedConversion[_]] = listExprs.splice.iterator

	  sb.append(textParts.next())

	  while(textParts.hasNext) {
	    sb.append(expressions.next.value)
	    sb.append(textParts.next)
	  }
	  Js(sb.toString)
	}
    }
  }

}

private[js] class JsStrings(sc: StringContext) {
  class JsContext() {
    def apply(exprs: ForcedConversion[Js]*): Js =
      macro JsMacros.contextMacro
  }
  val js = new JsContext()
}
