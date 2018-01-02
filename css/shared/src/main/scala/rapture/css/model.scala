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

sealed trait CssRule
case class CssCharset(encoding: String) extends CssRule {
  override def toString = s"@charset '$encoding';"
}
case class CssFontFace(properties: Css) extends CssRule {
  override def toString = s"@font-face { ${properties.content} }"
}
case class CssImport(href: String, media: String) extends CssRule {
  override def toString = s"@import url('$href') $media;"
}
case class CssMedia(cssRules: List[CssRule], media: String) extends CssRule {
  override def toString = s"@media $media { ${cssRules.mkString(" ")} }"
}
case class CssPage(selectorText: String, properties: Css) extends CssRule {
  override def toString = s"@page $selectorText { ${properties.content} }"
}
case class CssStyle(selectorText: String, properties: Css) extends CssRule {
  override def toString = s"$selectorText { ${properties.content} }"
}
case class CssUnknown(text: String) extends CssRule {
  override def toString = text
}

