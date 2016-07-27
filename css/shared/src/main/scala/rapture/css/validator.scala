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

import rapture.core._

import com.steadystate.css.parser._
import org.w3c.css.sac._
import org.w3c.dom.css._
import java.io._

import scala.collection.immutable.ListMap

object CssParser {

  case class ValidationException(strNo: Int, pos: Int, msg: String) extends Exception

  def parseStylesheet(parts: List[String], substitutions: List[String]): CssStylesheet = {
    val errHandler = new ErrorHandler {
      def error(e: CSSParseException) = throw ValidationException(0, e.getColumnNumber - 1, e.getMessage)
      def fatalError(e: CSSParseException) = error(e)
      def warning(e: CSSParseException) = error(e)
    }
    
    val source = new InputSource(new StringReader(parts.zip(substitutions :+ "").map { case (k, v) => k+v }.mkString))
    val parser = new CSSOMParser(new SACParserCSS3())
    parser.setErrorHandler(errHandler)

    val stylesheet = CssStylesheet(convertStylesheet(parser.parseStyleSheet(source, null, null)))
    stylesheet.rules.foreach(checkRule)
    stylesheet
  }

  def checkRule(cssRule: CssRule): Unit = cssRule match {
    case CssFontFace(css) => checkProperties(css)
    case CssMedia(rules, _) => rules.foreach(checkRule)
    case CssPage(_, css) => checkProperties(css)
    case CssStyle(_, css) => checkProperties(css)
    case _ => ()
  }

  def parse(parts: List[String], substitutions: List[String]): Css = {
    val errHandler = new ErrorHandler {
      def error(e: CSSParseException) = throw ValidationException(0, e.getColumnNumber - 1, e.getMessage)
      def fatalError(e: CSSParseException) = error(e)
      def warning(e: CSSParseException) = error(e)
    }
    val source = new InputSource(new StringReader(parts.zip(substitutions :+ "").map { case (k, v) => k+v }.mkString))
    val parser = new CSSOMParser(new SACParserCSS3())
    parser.setErrorHandler(errHandler)

    val styles = convertStyleDeclaration(parser.parseStyleDeclaration(source))
    checkProperties(styles)
    styles
  }

  def checkProperties(css: Css) = {
    for((k, v) <- css.properties) if (!Properties.all.contains(k)) throw ValidationException(0, 0, s"invalid CSS attribute '$k'")
  }
  def convertStylesheet(cssStylesheet: CSSStyleSheet): List[CssRule] = {
    val rules = cssStylesheet.getCssRules()
    (0 until rules.getLength).map { r => convertRule(rules.item(r)) }.to[List]
  }

  def convertRule(rule: CSSRule): CssRule = rule match {
    case rule: CSSCharsetRule => CssCharset(rule.getEncoding)
    case rule: CSSFontFaceRule => CssFontFace(convertStyleDeclaration(rule.getStyle))
    case rule: CSSImportRule => CssImport(rule.getHref, rule.getMedia.getMediaText)
    case rule: CSSMediaRule =>
      val rules = rule.getCssRules
      val convertedRules = (0 until rules.getLength).map { r => convertRule(rules.item(r)) }.to[List]
      CssMedia(convertedRules, rule.getMedia.getMediaText)
    case rule: CSSPageRule => CssPage(rule.getSelectorText, convertStyleDeclaration(rule.getStyle))
    case rule: CSSStyleRule => CssStyle(rule.getSelectorText, convertStyleDeclaration(rule.getStyle))
    case rule: CSSUnknownRule => CssUnknown(rule.getCssText)
  }

  def convertStyleDeclaration(styleDecl: CSSStyleDeclaration): Css = {
    val decls = (0 until styleDecl.getLength).map(styleDecl.item(_)).map { p => p -> styleDecl.getPropertyValue(p) }
    Css(ListMap(decls: _*))
  }
}
