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

import java.time._

import rapture.core.ParseException
import rapture.xml._
import rapture.test._
import rapture.xml.xmlBackends.stdlib
import rapture.core.java8.time._

class XmlJava8TimeApiTestRun extends Programme {
  include(StdlibTests)
  object StdlibTests extends XmlJava8TimeApiTests(stdlib.implicitXmlAst, stdlib.implicitXmlStringParser)
}


abstract class XmlJava8TimeApiTests(ast: XmlAst, parser: Parser[String, XmlAst]) extends TestSuite {

  implicit def implicitAst: XmlAst = ast

  implicit def implicitParser: Parser[String, XmlAst] = parser

  case class TestLocalDate(time: LocalDate)
  case class TestLocalDateTime(time: LocalDateTime)
  case class TestLocalTime(time: LocalTime)
  case class TestZonedDateTime(time: ZonedDateTime)
  case class TestOffsetDateTime(time: OffsetDateTime)
  case class TestOffsetTime(time: OffsetTime)


  val `[Java 8 Time Extractor] Extract LocalDate` = test {
    xml"""<time>2017-01-23</time>""".as[LocalDate]
  } returns LocalDate.of(2017, 1, 23)

  val `[Java 8 Time Extractor] Extract LocalDateTime` = test {
    xml"""<time>2017-01-23T18:28:51.045</time>""".as[LocalDateTime]
  } returns LocalDateTime.of(2017, 1, 23, 18, 28, 51, 45000000)

  val `[Java 8 Time Extractor] Extract LocalTime` = test {
    xml"""<time>18:38:14.997</time>""".as[LocalTime]
  } returns LocalTime.of(18, 38, 14, 997000000)


  val `[Java 8 Time Extractor] Extract ZonedDateTime` = test {
    xml"""<time>2017-01-23T18:41:02.086+02:00[Europe/Kiev]</time>""".as[ZonedDateTime]
  } returns ZonedDateTime.of(2017, 1, 23, 18, 41, 2, 86000000, ZoneId.of("Europe/Kiev"))


  val `[Java 8 Time Extractor] Extract OffsetDateTime` = test {
    xml"""<time>2017-01-23T18:44:18.221+02:00</time>""".as[OffsetDateTime]
  } returns OffsetDateTime.of(2017, 1, 23, 18, 44, 18, 221000000, ZoneOffset.ofHours(2))


  val `[Java 8 Time Extractor] Extract OffsetTime` = test {
    xml"""<time>19:03:25.325+02:00</time>""".as[OffsetTime]
  } returns OffsetTime.of(19, 3, 25, 325000000, ZoneOffset.ofHours(2))


  val `[Java 8 Time Extractor] Extract Failure` = test {
    xml"""<time>19:03:25.325+02:0</time>""".as[OffsetTime]
  } throws classOf[ParseException]


  val `[Java 8 Time Serializator] Serialize LocalDate` = test {
    Xml(TestLocalDate(LocalDate.of(2017, 1, 23))).toBareString
  } returns xml"""<time>2017-01-23</time>""".toBareString

  val `[Java 8 Time Serializator] Serialize LocalDateTime` = test {
    Xml(TestLocalDateTime( LocalDateTime.of(2017, 1, 23, 18, 28, 51, 45000000))).toBareString
  } returns xml"""<time>2017-01-23T18:28:51.045</time>""".toBareString


  val `[Java 8 Time Serializator] Serialize LocalTime` = test {
    Xml(TestLocalTime(LocalTime.of(18, 38, 14, 997000000))).toBareString
  } returns xml"""<time>18:38:14.997</time>""".toBareString


  val `[Java 8 Time Serializator] Serialize ZonedDateTime` = test {
    Xml(TestZonedDateTime(ZonedDateTime.of(2017, 1, 23, 18, 41, 2, 86000000, ZoneId.of("Europe/Kiev")))).toBareString
  } returns xml"""<time>2017-01-23T18:41:02.086+02:00[Europe/Kiev]</time>""".toBareString


  val `[Java 8 Time Serializator] Serialize OffsetDateTime` = test {
    Xml(TestOffsetDateTime(OffsetDateTime.of(2017, 1, 23, 18, 44, 18, 221000000, ZoneOffset.ofHours(2)))).toBareString
  } returns xml"""<time>2017-01-23T18:44:18.221+02:00</time>""".toBareString


  val `[Java 8 Time Serializator] Serialize OffsetTime` = test {
    Xml(TestOffsetTime(OffsetTime.of(19, 3, 25, 325000000, ZoneOffset.ofHours(2)))).toBareString
  } returns xml"""<time>19:03:25.325+02:00</time>""".toBareString

}