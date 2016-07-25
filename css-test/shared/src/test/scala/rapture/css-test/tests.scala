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

package rapture.css.test

import rapture.core._
import rapture.css._
import rapture.html._
import rapture.test._

class TestRun extends Programme {
  include(CssTests)
}

object CssTests extends TestSuite {
  val `Embed ID in CSS` = test {
    val identifier = DomId.auto
    cssStylesheet"""$identifier { color: red; }"""
  } returns cssStylesheet"""#identifier { color: red; }"""
  
  val `Embed class in CSS` = test {
    val identifier = CssClass.auto
    cssStylesheet"""$identifier { color: red; }"""
  } returns cssStylesheet""".identifier { color: red; }"""
  
  /*val `Embed properties in CSS` = test {
    val properties = css"border-width: 1px; color: red"
    cssStylesheet""".class { foo: bar; $properties }"""
  } returns cssStylesheet""".class { border-width: 1px; color: red; }"""*/
  
  val `Embed HTML tag in CSS` = test {
    
    import htmlSyntax._
    
    cssStylesheet"""$Div { color: red; }"""
  } returns cssStylesheet"""div { color: red; }"""
  
  val `Check stylesheet well-formedness` = test {
    
    cssStylesheet"""div { foo: red; }"""
  } returns cssStylesheet"""div { foo: red; }"""
}

