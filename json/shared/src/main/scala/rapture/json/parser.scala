package rapture.json

import scala.annotation.implicitNotFound


@implicitNotFound(msg = "Cannot find ${Ast} parser for values of type ${Source}")
trait Parser[-Source, +Ast <: JsonAst] {
  val ast: Ast
  def parse(s: Source): Option[Any]
}
