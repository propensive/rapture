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

package rapture.json.test

import rapture.core._
import rapture.json._
import rapture.data.{Parser, DataTypes}
import rapture.test._

import scala.util

class TestRun extends Programme {
  include(PlayTests)
  include(JawnTests)
  include(Json4sTests)
  include(SprayTests)
  include(JacksonTests)
  include(ArgonautTests)
  include(CirceTests)
  include(LiftTests)
  //include(MutablePlayTests)
  //include(MutableJawnTests)
  //include(MutableJson4sTests)
  //include(MutableSprayTests)
  //include(MutableArgonautTests)
  //include(MutableCirceTests)
  //include(MutableLiftTests)
}

import jsonBackends._

object PlayTests extends JsonTests(play.implicitJsonAst, play.implicitJsonStringParser)
object JawnTests extends JsonTests(jawn.implicitJsonAst, jawn.implicitJsonStringParser(jawn.jawnFacade))
object Json4sTests extends JsonTests(json4s.implicitJsonAst, json4s.implicitJsonStringParser)
object SprayTests extends JsonTests(spray.implicitJsonAst, spray.implicitJsonStringParser)
object JacksonTests extends JsonTests(jackson.implicitJsonAst, jackson.implicitJsonStringParser)
object ArgonautTests extends JsonTests(argonaut.implicitJsonAst, argonaut.implicitJsonStringParser)
object CirceTests extends JsonTests(circe.implicitJsonAst, circe.implicitJsonStringParser)
object LiftTests extends JsonTests(lift.implicitJsonAst, lift.implicitJsonStringParser)

object MutablePlayTests extends MutableJsonTests(play.implicitJsonAst, play.implicitJsonStringParser)
object MutableJawnTests extends MutableJsonTests(jawn.implicitJsonAst, jawn.implicitJsonStringParser(jawn.jawnFacade))
object MutableJson4sTests extends MutableJsonTests(json4s.implicitJsonAst, json4s.implicitJsonStringParser)
object MutableSprayTests extends MutableJsonTests(spray.implicitJsonAst, spray.implicitJsonStringParser)
object MutableArgonautTests extends MutableJsonTests(argonaut.implicitJsonAst, argonaut.implicitJsonStringParser)
object MutableCirceTests extends MutableJsonTests(circe.implicitJsonAst, circe.implicitJsonStringParser)
object MutableLiftTests extends MutableJsonTests(lift.implicitJsonAst, lift.implicitJsonStringParser)


case class Foo(alpha: String, beta: Int)
case class Bar(foo: Foo, gamma: Double)

case class Baz(alpha: String, beta: Option[Int])
case class Baz2(alpha: String, beta: util.Try[Int])

case class HasDefault(alpha: String = "yes", beta: Int)
case class HasDefault2(alpha: String, beta: Int = 1)

case class A(a: B)
case class B(b: C)
case class C(c: D)
case class D(d: E)
case class E(e: F)
case class F(f: Int)

abstract class JsonTests(ast: JsonAst, parser: Parser[String, JsonAst]) extends TestSuite {

  implicit def implicitAst: JsonAst = ast
  implicit def implicitParser: Parser[String, JsonAst] = parser

  val source1 = json"""{
    "string": "Hello",
    "int": 42,
    "double": 3.14159,
    "boolean": true,
    "list": [1, 2, 3],
    "foo": { "alpha": "test", "beta": 1 },
    "bar": { "foo": { "alpha": "test2", "beta": 2 }, "gamma": 2.7 },
    "baz": { "alpha": "test" },
    "baz2": { "alpha": "test", "beta": 7 },
    "self": 0,
    "boo": null,
    "booInner": {"foo": null, "bar": "value"}
  }"""

  val `Extract Int` = test {
    source1.int.as[Int]
  } returns 42
  val `Extract value called "self"` = test {
    source1.self.as[Int]
  } returns 0
  
  val `Extract Option[Int]` = test {
    source1.int.as[Option[Int]]
  } returns Some(42)
  
  val `Extract Option[Int], wrong type` = test {
    source1.string.as[Option[Int]]
  } returns None
 
  val `Extract String` = test {
    source1.string.as[String]
  } returns "Hello"
  
  val `Extract Double` = test {
    source1.double.as[Double]
  } returns 3.14159
  
  val `Extract Boolean` = test {
    source1.boolean.as[Boolean]
  } returns true
  
  val `Extract List[Int]` = test {
    source1.list.as[List[Int]]
  } returns List(1, 2, 3)
  
  val `Extract Vector[Int]` = test {
    source1.list.as[Vector[Int]]
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
    json"""{ "a": { "b": { "c": { "d": { "e": { "f": 1 } } } } } }""".as[A]
  } returns A(B(C(D(E(F(1))))))

  val `Extract List element` = test {
    source1.list(1).as[Int]
  } returns 2
  
  val `Extract object element` = test {
    source1.bar.foo.alpha.as[String]
  } returns "test2"

  val `Extract null element` = test {
    source1.boo.as[Null]
  } returns null

  val `Extract null element from inner value` = test {
    source1.booInner.foo.as[Null]
  } returns null

  val `Try to extract null element from not null inner value` = test {
    source1.booInner.bar.as[Null]
  } throws TypeMismatchException(DataTypes.String, DataTypes.Null)

  val `Try to extract null element from not null value` = test {
    source1.double.as[Null]
  } throws TypeMismatchException(DataTypes.Number, DataTypes.Null)

  val `Match null element` = test {
    source1 match {
      case json""" { "boo": $h } """ => h.as[Null]
    }
  } returns null

  val `Match inner null element` = test {
    source1 match {
      case json""" { "booInner": {"foo": $h } } """ => h.as[Null]
    }
  } returns null

  val `Extract long element` = test {
    source1.int.as[Long]
  } returns 42L
  // For some reason these two tests work fine in the REPL, but not here.
  /*
  val `Extract missing value with case class default` = test {
    json"""{"beta": 0}""".as[HasDefault]
  } returns HasDefault("yes", 0)
  
  val `Extract missing value with case class default 2` = test {
    json"""{"alpha": "no"}""".as[HasDefault2]
  } returns HasDefault2("no", 1)
  */
  
  val `Extract case class ignoring default value` = test {
    json"""{"alpha": "no", "beta": 0}""".as[HasDefault2]
  } returns HasDefault2("no", 0)
  
  val `Check type failure` = test {
    source1.string.as[Int]
  } throws TypeMismatchException(DataTypes.String, DataTypes.Number)

  val `Check missing value failure` = test {
    source1.nothing.as[Int]
  } throws MissingValueException()

  val `Match string` = test {
    source1 match {
      case json""" { "string": $h } """ => h.as[String]
    }
  } returns "Hello"

  val `Match inner JSON` = test {
    source1 match {
      case json""" { "foo": $foo } """ => foo
    }
  } returns json"""{ "alpha": "test", "beta": 1 }"""
  
  val `Match inner string` = test {
    source1 match {
      case json""" { "foo": { "alpha": $t } } """ => t.as[String]
    }
  } returns "test"
  
  val `Filtered match` = test {
    source1 match {
      case json""" { "int": 42, "foo": { "alpha": $t } } """ => t.as[String]
    }
  } returns "test"
  
  val `Inner filtered match` = test {
    source1 match {
      case json""" { "foo": { "alpha": "test" }, "bar": { "gamma": $g } } """ => g.as[Double]
    }
  } returns 2.7
  
  val `Filtered failed match` = test {
    source1 match {
      case json""" { "int": 0, "foo": { "alpha": $t } } """ => t.as[String]
    }
  } throws classOf[MatchError]

  val `Multiple pattern match` = test {
    json"""{ "foo": "bar" }""" match {
      case json"""{ "bar": "foo" }""" => 0
      case json"""{ "foo": "baz" }""" => 1
      case json"""{ "foo": "bar" }""" => 2
    }
  } returns 2

  val `Empty object doesn't match` = test {
    json"""{ "foo": "bar" }""" match {
      case json"""{ "foo": {} }""" => 0
    }
  } throws classOf[MatchError]

  val `Serialize string` = test {
    Json("Hello World!").toString
  } returns "json\"\"\"\"Hello World!\"\"\"\""

  val `Serialize int` = test {
    Json(1648).toString
  } returns "json\"\"\"1648\"\"\""

  val `Serialize array` = test {
    Json(List(1, 2, 3)).toString
  } returns "json\"\"\"[1,2,3]\"\"\""

  val `Serialize object` = test {
    import formatters.humanReadable._
    Json.format(json"""{"baz":"quux","foo":"bar"}""")
  } returns """{
             | "baz": "quux",
             | "foo": "bar"
             |}""".stripMargin
  
  val `Empty object serialization` = test {
    import formatters.humanReadable._
    Json.format(json"{}")
  } returns "{}"
  
  val `Empty array serialization` = test {
    import formatters.humanReadable._
    Json.format(json"[]")
  } returns "[]"

  // As reported by Jim Newsham
  val `Extracting Option should not throw exception` = test {
    val j = json"""{"foo":"bar"}"""
    j.as[Option[String]]
  } returns None

  // Reported by @ajrnz
  val `Tabs should be escaped when serializing strings` = test {
    Json("\t").toString
  } returns "json\"\"\"\"\\t\"\"\"\""

  val `Extract Byte` = test {
    val j = json"""{ "foo": 127 }"""
    j.foo.as[Byte]
  } returns (127.toByte)

  val `Extract Short` = test {
    val j = json"""{ "foo": 12345 }"""
    j.foo.as[Short]
  } returns (12345.toShort)

  val `Extract Long` = test {
    val j = json"""{ "foo": 1234567890123456789 }"""
    j.foo.as[Long]
  } returns 1234567890123456789L

}

abstract class MutableJsonTests(ast: JsonBufferAst, parser: Parser[String, JsonBufferAst]) extends TestSuite {
 
  implicit def implicitAst: JsonBufferAst = ast
  implicit def implicitParser: Parser[String, JsonBufferAst] = parser

  case class Foo(alpha: String, beta: Int)
  case class Bar(foo: Foo, gamma: Double)
  
  val mutableSource = jsonBuffer"""{
    "string": "Hello",
    "int": 42,
    "double": 3.14159,
    "boolean": true,
    "list": [1, 2, 3],
    "foo": { "alpha": "test", "beta": 1 },
    "bar": { "foo": { "alpha": "test2", "beta": 2 }, "gamma": 2.7 },
    "baz": { "alpha": "test" },
    "baz2": { "alpha": "test", "beta": 7 },
    "self": 0
  }"""

  val `Mutable extract Int` = test {
    mutableSource.int.as[Int]
  } returns 42
  
  val source2 = JsonBuffer.parse("""{
    "string": "Hello",
    "int": 42
  }""")

  val `Mutable get String` = test {
    source2.string.as[String]
  } returns "Hello"

  //val `Mutable get optional String` = test {
  //  source2.string.as[Option[String]]
  //} returns Some("Hello")

  val `Mutable get Int` = test {
    source2.int.as[Int]
  } returns 42

  val `Mutable change String` = test {
    source2.string = "World"
    source2.string.as[String]
  } returns "World"

  val `Mutable add String` = test {
    source2.inner.newString = "Hello"
    source2.inner.newString.as[String]
  } returns "Hello"
 
  val `Mutable add Json` = test {
    val jb = JsonBuffer.empty
    jb.foo = json"""{ "foo": "bar" }"""
  } returns jsonBuffer"""{ "foo": { "foo": "bar" } }"""

  val `Mutable add case class` = test {
    source2.foo = Foo("string", -1)
    source2.foo.as[Foo]
  } returns Foo("string", -1)
 
  val `Deep insertion of integer` = test {
    source2.alpha.beta.gamma.delta = 1
    source2.alpha.beta.gamma.delta.as[Int]
  } returns 1

  val `Array autopadding` = test {
    source2.autopad(4) = 1
    source2.autopad(4).as[Int]
  } returns 1

  val `Deep array insertion of integer` = test {
    source2.array(1)(2)(3)(4) = 1
    source2.array(1)(2)(3)(4).as[Int]
  } returns 1

  val `Deep mixed insertion of string` = test {
    source2.mixed(4).foo.bar(2).baz = "Mixed"
    source2.mixed(4).foo.bar(2).baz.as[String]
  } returns "Mixed"

  val `Mutable add array String` = test {
    source2.inner.newArray += "Hello"
    source2.inner.newArray(0).as[String]
  } returns "Hello"
}
