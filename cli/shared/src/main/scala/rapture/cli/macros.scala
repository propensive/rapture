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

package rapture.cli

import rapture.base._
import rapture.core._

private[cli] object CliMacros {

  def shImplementation(c: BlackboxContext)(content: c.Expr[ShParam]*): c.Expr[Process] = {
    import c.universe._

    val params = c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        val parts = rawParts.to[Vector].zip(content.map(_.tree).to[Vector]).flatMap {
          case (x, y) => Vector(x, y)
        } :+ rawParts.last

        var params: Vector[c.Tree] = Vector()
        var param: Vector[Either[String, c.Tree]] = Vector()
        var inline: Boolean = false
        var singleQuoted: Boolean = false
        var doubleQuoted: Boolean = false
        var escaped: Boolean = false

        def add(chr: Char) = {
          param =
            if (param.isEmpty) Vector(Left(chr.toString))
            else
              param.last match {
                case Right(_) =>
                  param :+ Left(chr.toString)
                case Left(str) =>
                  param.init :+ Left(str + chr)
              }
          escaped = false
        }

        def nextParam() = if (!param.isEmpty) {

          val next: c.Tree = if (inline) {
            val strings = param.map {
              case Left(str) =>
                q"$str"
              case Right(tr) =>
                q"""$tr.elems.mkString(" ")"""
            }
            q"_root_.scala.Vector(_root_.scala.Vector(_root_.scala.Vector(..$strings).mkString))"
          } else {
            val values = param.map {
              case Left(str) =>
                q"_root_.scala.Vector($str)"
              case Right(tr) =>
                q"$tr.elems"
            }
            q"_root_.scala.Vector(..$values)"
          }

          params = params :+ next
          param = Vector()
          inline = false
        }

        parts.foreach {
          case Literal(Constant(str: String)) =>
            str.foreach {
              case chr if escaped =>
                add(chr)
              case ' ' =>
                if (singleQuoted || doubleQuoted) add(' ') else nextParam()
              case '\\' =>
                escaped = true
              case '\'' if !doubleQuoted =>
                singleQuoted = !singleQuoted
              case '"' if !singleQuoted =>
                doubleQuoted = !doubleQuoted
              case chr =>
                add(chr)
            }
          case tr =>
            inline = inline || singleQuoted || doubleQuoted
            param = param :+ Right(tr.asInstanceOf[c.Tree])
        }

        nextParam()

        if (singleQuoted || doubleQuoted) c.abort(c.enclosingPosition, "unclosed quoted parameter")
        if (params.isEmpty) c.abort(c.enclosingPosition, "no command specified")

        q"_root_.scala.Vector(..$params).flatten.flatten"
    }

    c.Expr(q"""new _root_.rapture.cli.Process($params)""")
  }
}
