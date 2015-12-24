/******************************************************************************************************************\
* Rapture HTML, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
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
package rapture.html

import rapture.uri._

object Tests {

  import htmlSyntax._

  def `Div can contain P` = Div(P)

  def `Html can contain Head` = Html(Head)

  def `Html can contain Body` = Html(Body)

  def `Table can contain Tbody/Thead` = Table(Thead, Tbody)

  def `Tbody can contain Tr` = Tbody(Tr, Tr, Tr)

  def `Tr can contain Th/Tr` = Tr(Th, Td, Td, Td)

  def `Div with id can contain P` = Div(id = 'foo)(P)

  def `Html with id can contain Head` = Html(id = 'foo)(Head)

  def `Html with id can contain Body` = Html(id = 'foo)(Body)

  def `Table with id can contain Tbody/Thead` = Table(id = 'foo)(Thead, Tbody)

  def `Tbody with id can contain Tr` = Tbody(id = 'foo)(Tr, Tr, Tr)

  def `Tr with id can contain Th/Tr` = Tr(id = 'foo)(Th, Td, Td, Td)
 
  def `Img has src attribute` = Img(src = ^ / "foo.jpg")

  //def `Should fail` = Html(src = "foo")

  def `Get Tds` = {
    val table = Table(Tbody(Tr(Td, Td, Td), Tr(Td, Td, Td)))
    println(table \ Tbody \ Tr \ Td)
  }

}
