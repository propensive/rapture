package rapture.xml

import scala.annotation.implicitNotFound


@implicitNotFound(msg = "Cannot find ${Ast} parser for values of type ${Source}")
trait Parser[-Source, +Ast <: XmlAst] {
  val ast: Ast
  def parse(s: Source): Option[Any]
}
