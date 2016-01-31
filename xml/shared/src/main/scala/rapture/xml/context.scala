/******************************************************************************************************************\
* Rapture XML, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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
package rapture.xml

import rapture.base._
import rapture.core._
import rapture.data._

import language.experimental.macros

private[xml] object XmlDataMacros extends DataContextMacros[Xml, XmlAst] {
  
  def companion(c: BlackboxContext): c.Expr[DataCompanion[Xml, XmlAst]] = c.universe.reify(Xml)

  def parseSource(s: List[String], stringsUsed: List[Boolean]) = try {
    XmlValidator.validate(s)
    None
  } catch {
    case XmlValidator.ValidationException(strNo, pos, expected, found) =>
      val f = if(found == '\u0000') "end of input" else s"'$found'"
      Some((strNo, pos, s"failed to parse Xml literal: expected $expected, but found $f"))
    case XmlValidator.DuplicateKeyException(strNo, pos, key) =>
      Some((strNo, pos, s"""duplicate key found in Xml literal: "$key""""))
  }
  
  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[Xml]]*)
      (parser: c.Expr[Parser[String, XmlAst]]): c.Expr[Xml] =
    super.contextMacro(c)(exprs: _*)(parser)

}

private[xml] object XmlBufferDataMacros extends DataContextMacros[XmlBuffer, XmlBufferAst] {
  
  def companion(c: BlackboxContext): c.Expr[DataCompanion[XmlBuffer, XmlBufferAst]] =
    c.universe.reify(XmlBuffer)

  def parseSource(s: List[String], stringsUsed: List[Boolean]) = try {
    XmlValidator.validate(s)
    None
  } catch {
    case XmlValidator.ValidationException(strNo, pos, expected, found) =>
      val f = if(found == '\u0000') "end of input" else s"'$found'"
      Some((strNo, pos,
          s"Failed to parse XmlBuffer literal: Expected $expected, but found $f."))
  }
  
  override def contextMacro(c: BlackboxContext)(exprs: c.Expr[ForcedConversion[XmlBuffer]]*)
      (parser: c.Expr[Parser[String, XmlBufferAst]]): c.Expr[XmlBuffer] =
    super.contextMacro(c)(exprs: _*)(parser)
}

/** Provides support for XML literals, in the form xml" { } " or xml""" { } """.
  * Interpolation is used to substitute variable names into the XML, and to extract values
  * from a XML string. */
private[xml] class XmlStrings(sc: StringContext) {
  class XmlContext() extends DataContext(Xml, sc) {
    def apply(exprs: ForcedConversion[Xml]*)(implicit parser: Parser[String, XmlAst]): Xml =
      macro XmlDataMacros.contextMacro
  }
  val xml = new XmlContext()
}

private[xml] class XmlBufferStrings(sc: StringContext) {
  class XmlBufferContext() extends DataContext(XmlBuffer, sc) {
    def apply(exprs: ForcedConversion[XmlBuffer]*)(implicit parser: Parser[String,
        XmlBufferAst]): XmlBuffer =
      macro XmlBufferDataMacros.contextMacro
  }
  val xmlBuffer = new XmlBufferContext()
}
