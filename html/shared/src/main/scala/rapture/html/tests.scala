/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.html

import rapture.uri._
import rapture.net._

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

  def `Link has hreflang attribute` = Link(hreflang = 'en)

  def `Link has sizes attribute` = Link(sizes = "16x16")

  def `Style has scoped attribute` = Style(scoped = true)

  def `Script has async attribute` = Script(async = true)

  def `Script has defer attribute` = Script(defer = true)

  def `Body has onbeforeunload attribute` = Body(onbeforeunload = "foo()")

  def `Body has onerror attribute` = Body(onerror = "foo()")

  def `Body has onhashchange attribute` = Body(onhashchange = "foo()")

  def `Body has onmessage attribute` = Body(onmessage = "foo()")

  def `Body has onoffline attribute` = Body(onoffline = "foo()")

  def `Body has onpopstate attribute` = Body(onpopstate = "foo()")

  def `Body has onresize attribute` = Body(onresize = "foo()")

  def `Body has onstorage attribute` = Body(onstorage = "foo()")

  def `Body has onunload attribute` = Body(onunload = "foo()")

  def `Ol has reversed attribute` = Ol(reversed = true)

  def `Ol has start attribute` = Ol(start = 10)

  def `Link has ping attribute` = Link(ping = uri"http://foo/bar")

  def `Blockquote has cite attribute` = Blockquote(cite = uri"http://foo/bar")

  def `Time has datetime attribute` = Time(datetime = "2008-02-14")

  def `P has dir attribute` = P(dir = 'rtl)

  def `Img has alt attribute` = Img(alt = "foo")

  def `Img has usemap attribute` = Img(usemap = "#foo")

  def `Img has ismap attribute` = Img(ismap = true)

  def `Iframe has width attribute` = Iframe(width = 200)

  def `Iframe has height attribute` = Iframe(height = 300)

  def `Iframe has sandbox attribute` = Iframe(sandbox = true)

  def `Iframe has seamless attribute` = Iframe(seamless = true)

  def `Object has data attribute` = Object(data = "foo.bar")

  def `Video has poster attribute` = Video(poster = ^ / "foo.bar")

  def `Video has autobuffer attribute` = Video(autobuffer = true)

  def `Video has autoplay attribute` = Video(autoplay = true)

  def `Video has loop attribute` = Video(loop = true)

  def `Video has controls attribute` = Video(controls = true)

  def `Area has coords attribute` = Area(coords = "124,58,8")

  def `Area has shape attribute` = Area(shape = "124,58,8")

  def `Td has headers attribute` = Td(headers = 'foo)

  def `Th has scope attribute` = Th(scope = 'foo)

  def `Form has accept-charset attribute` = Form(acceptCharset = "ISO-8859-1")

  def `Input has autocomplete attribute` = Input(autocomplete = true)

  def `Form has novalidate attribute` = Form(novalidate = true)

  def `Command has label attribute` = Command(label = "foo")

  def `Command has forName attribute` = Command(forName = 'foo)

  def `Input has accept attribute` = Input(accept = "image/*")

  def `Input has autofocus attribute` = Input(autofocus = true)

  def `Input has list attribute` = Input(list = 'foo)

  def `Input has multiple attribute` = Input(multiple = true)

  def `Input has pattern attribute` = Input(pattern = "[A-Za-z]{3}")

  def `Input has placeholder attribute` = Input(placeholder = "foo")

  def `Input has readonly attribute` = Input(readonly = true)

  def `Input has required attribute` = Input(required = true)

  def `Input has size attribute` = Input(size = 20)

  def `Input has step attribute` = Input(step = 2)

  def `Command has icon attribute` = Command(icon = ^ / "foo.jpg")

  def `Command has radiogroup attribute` = Command(radiogroup = 'foo)

  def `Command has default attribute` = Command(default = "foo")

  def `Label has for attribute` = Label(`for` = 'foo)

  def `Fieldset can contain Legend` = Fieldset(Legend("foo"))

  //def `Should fail` = Html(src = "foo")

  def `Get Tds` = {
    val table = Table(Tbody(Tr(Td, Td, Td), Tr(Td, Td, Td)))
    println(table \ Tbody \ Tr \ Td)
  }

}
