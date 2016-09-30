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

package rapture.xml.test

import rapture.core._
import rapture.xml._
import rapture.data.Parser
import rapture.test._

import scala.util

class TestRun extends Programme {
  include(StdlibTests)
}

private[test] case class Foo(alpha: String, beta: Int)
private[test] case class Bar(foo: Foo, gamma: Double)

private[test] case class Baz(alpha: String, beta: Option[Int])
private[test] case class Baz2(alpha: String, beta: util.Try[Int])

private[test] case class HasDefault(alpha: String = "yes", beta: Int)
private[test] case class HasDefault2(alpha: String, beta: Int = 1)

private[test] case class A(a: B)
private[test] case class B(b: C)
private[test] case class C(c: D)
private[test] case class D(d: E)
private[test] case class E(e: F)
private[test] case class F(f: Int)

import xmlBackends._
object StdlibTests extends XmlTests(stdlib.implicitXmlAst, stdlib.implicitXmlStringParser)

class MutableStdlibTests() extends MutableXmlTests(stdlib.implicitXmlAst, stdlib.implicitXmlStringParser)

abstract class XmlTests(ast: XmlAst, parser: Parser[String, XmlAst]) extends TestSuite {

  implicit def implicitAst: XmlAst = ast
  implicit def implicitParser: Parser[String, XmlAst] = parser

  val source1 = xml"""<source>
    <string>Hello</string>
    <int>42</int>
    <double>3.14159</double>
    <boolean>true</boolean>
    <list>
      <item>1</item>
      <item>2</item>
      <item>3</item>
    </list>
    <foo>
      <alpha>test</alpha>
      <beta>1</beta>
    </foo>
    <bar>
      <foo>
        <alpha>test2</alpha>
        <beta>2</beta>
      </foo>
      <gamma>2.7</gamma>
    </bar>
    <baz>
      <alpha>test</alpha>
    </baz>
    <baz2>
      <alpha>test</alpha>
      <beta>7</beta>
    </baz2>
    <self>0</self>
  </source>"""

  val `Extract Int` = test {
    source1.int.as[Int]
  } returns 42

  val `Extract Option[Int]` = test {
    source1.int.as[Option[Int]]
  } returns Some(42)

  val `Extract Option[Int], wrong type` = test {
    source1.string.as[Option[Int]]
  } returns None

  val `Extract Double` = test {
    source1.double.as[Double]
  } returns 3.14159

  val `Extract Boolean` = test {
    source1.boolean.as[Boolean]
  } returns true

  val `Extract String` = test {
    source1.string.as[String]
  } returns "Hello"

  val `Extract List[Int]` = test {
    source1.list.item.as[List[Int]]
  } returns List(1, 2, 3)

  val `Extract Vector[Int]` = test {
    source1.list.item.as[Vector[Int]]
  } returns Vector(1, 2, 3)

  val `Extract case class` = test {
    source1.foo.as[Foo]
  } returns Foo("test", 1)

  val `Extract case class with missing optional value` = test {
    source1.baz.as[Baz]
  } returns Baz("test", None)

  val `Extract case class with missing tried value` = test {
    source1.baz.as[Baz2]
  } returns Baz2("test", util.Failure(MissingValueException()))

  val `Extract case class with present optional value` = test {
    source1.baz2.as[Baz]
  } returns Baz("test", Some(7))

  val `Extract case class with present tried value` = test {
    source1.baz2.as[Baz2]
  } returns Baz2("test", util.Success(7))

  val `Extract nested case class` = test {
    source1.bar.as[Bar]
  } returns Bar(Foo("test2", 2), 2.7)

  val `Extract deeply-nested case class` = test {
    xml"""<obj><a><b><c><d><e><f>1</f></e></d></c></b></a></obj>""".as[A]
  } returns A(B(C(D(E(F(1))))))

  val `Extract List element` = test {
    source1.list.item(1).as[Int]
  } returns 2

  val `Extract object element` = test {
    source1.bar.foo.alpha.as[String]
  } returns "test2"

  val `Extract missing value with case class default` = test {
    xml"""<obj><beta>0</beta></obj>""".as[HasDefault]
  } returns HasDefault("yes", 0)

  val `Extract missing value with case class default 2` = test {
    xml"""<obj><alpha>no</alpha></obj>""".as[HasDefault2]
  } returns HasDefault2("no", 1)

  val `Extract case class ignoring default value` = test {
    xml"""<obj><beta>0</beta><alpha>no</alpha></obj>""".as[HasDefault2]
  } returns HasDefault2("no", 0)

  val `Check type failure` = test {
    source1.string.as[Int]
  } throws InvalidNumber("Hello", "integer")

  val `Check missing value failure` = test {
    source1.nothing.as[Int]
  } throws MissingValueException()

  // FIXME: Add pattern-matching tests

  val `Serialize string` = test {
    Xml("Hello World!").toString
  } returns "Hello World!"

  val `Serialize int` = test {
    Xml(1648).toString
  } returns "xml\"\"\"1648\"\"\""

  /*val `Serialize array` = test {
    Json(List(1, 2, 3)).toString
  } returns "123"

  val `Serialize object` = test {
    import formatters.humanReadable._
    Xml.format(xml"<baz>quux</baz><foo>bar</foo>")
  } returns "<baz>quux</baz><foo>bar</foo>"

  val `Empty object serialization` = test {
    import formatters.humanReadable._
    Json.format(json"{}")
  } returns "{}"

  val `Empty node serialization` = test {
    import formatters.humanReadable._
    Xml.format(xml"<empty></empty>")
  } returns "<empty/>"

  val `Extracting Option should not throw exception` = test {
    val x = xml"""{"foo":"bar"}"""
    j.as[Option[String]]
  } returns None*/

}

abstract class MutableXmlTests(ast: XmlBufferAst, parser: Parser[String, XmlBufferAst]) extends TestSuite {

  implicit def implicitAst: XmlBufferAst = ast
  implicit def implicitParser: Parser[String, XmlBufferAst] = parser

  case class Foo(alpha: String, beta: Int)
  case class Bar(foo: Foo, gamma: Double)

  val source1 = xmlBuffer"""<source>
    <string>Hello</string>
    <int>42</int>
    <double>3.14159</double>
    <boolean>true</boolean>
    <list>
      <item>1</item>
      <item>2</item>
      <item>3</item>
    </list>
    <foo>
      <alpha>test</alpha>
      <beta>1</beta>
    </foo>
    <bar>
      <foo>
        <alpha>test2</alpha>
        <beta>2</beta>
      </foo>
      <gamma>2.7</gamma>
    </bar>
    <baz>
      <alpha>test</alpha>
    </baz>
    <baz2>
      <alpha>test</alpha>
      <beta>7</beta>
    </baz2>
    <self>0</self>
  </source>"""
}
