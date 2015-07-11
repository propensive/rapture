/******************************************************************************************************************\
* Rapture URI, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/

package rapture.uri

import rapture.base._
import rapture.core._

import language.experimental.macros
import scala.reflect.macros._

object Paramable {
  implicit val stringParamable = new Paramable[String] { def paramize(s: String): String = s }
  implicit val intParamable = new Paramable[Int] { def paramize(s: Int): String = s.toString }
  implicit val doubleParamable = new Paramable[Double] { def paramize(s: Double): String = s.toString }
}

trait Paramable[T] {
  def paramize(t: T): String
}

object UriMacros {
 
  // FIXME: Check the purpose of this
  def paramsMacro[T: c.WeakTypeTag](c: BlackboxContext): c.Expr[QueryType[AnyPath, T]] = {
    import c.universe._
    import compatibility._

    require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)

    val paramable = typeOf[Paramable[_]].typeSymbol.asType.toTypeConstructor

    val params = declarations(c)(weakTypeOf[T]) collect {
      case m: MethodSymbol if m.isCaseAccessor => m.asMethod
    } map { p =>
      val implicitParamable = c.Expr[Paramable[_]](c.inferImplicitValue(appliedType(paramable, List(p.returnType)), false, false)).tree
      val paramValue = Apply(
        Select(
          implicitParamable,
          termName(c, "paramize")
        ),
        List(
          Select(
            Ident(termName(c, "t")),
            p.name
          )
        )
      )

      val paramName = Literal(Constant(p.name.toString+"="))
      
      Apply(
        Select(paramName, termName(c, "$plus")),
        List(paramValue)
      )
    }

    val listOfParams = c.Expr[List[String]](Apply(
      Select(
        Ident(termName(c, "List")),
        termName(c, "apply")
      ),
      params.to[List]
    ))

    reify {
      new QueryType[AnyPath, T] {
        def extras(existing: Map[Char, (String, Double)], t: T): Map[Char, (String, Double)] =
          existing ++ Map('?' -> (listOfParams.splice.mkString("&"), 1.0))
      }
    }
  }
 
  def uriMacro(c: WhiteboxContext)(content: c.Expr[String]*): c.Expr[Any] = {
    import c.universe._
    import compatibility._

    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => rawParts.head match {
        case Literal(Constant(part: String)) =>
          val scheme = part.split(":", 2) match {
            case Array(s, _) => s
            case _ => c.abort(c.enclosingPosition, "Could not find a valid scheme for this URI.")
          }
          
          val constants = rawParts.to[List] match {
            case Literal(Constant(h: String)) :: t =>
              Literal(Constant(h.substring(scheme.length + 1))) :: t
          }
          
          val variables = content.map(_.tree).to[List]
          c.Expr(q"_root_.rapture.uri.UriContext.${termName(c, scheme)}(List(..$constants))(List(..$variables))")
      }
    }
  }
}
