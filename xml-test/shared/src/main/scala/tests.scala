package rapture.xml.test

import rapture.core._
import rapture.xml._
import rapture.data.{Parser, DataTypes}
import rapture.test._

import scala.util

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
class StdlibTests() extends XmlTests(stdlib.implicitXmlAst, stdlib.implicitXmlStringParser)

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
  } returns Baz2("test", util.Failure(InvalidNumber("", "integer")))
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
