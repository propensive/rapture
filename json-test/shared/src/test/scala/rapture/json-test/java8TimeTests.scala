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

import java.time._

import rapture.core.ParseException
import rapture.data.Parser
import rapture.json.jsonBackends._
import rapture.json.{JsonAst, _}
import rapture.test.{Programme, TestSuite}
import rapture.core.java8.time._

class JsonJava8TimeApiTestRun extends Programme {
  include(PlayTests)
  include(JawnTests)
  include(Json4sTests)
  include(SprayTests)
  include(JacksonTests)
  include(ArgonautTests)
  include(CirceTests)
  include(LiftTests)

  object PlayTests extends JsonJava8TimeApiTests(play.implicitJsonAst, play.implicitJsonStringParser)

  object JawnTests extends JsonJava8TimeApiTests(jawn.implicitJsonAst, jawn.implicitJsonStringParser(jawn.jawnFacade))

  object Json4sTests extends JsonJava8TimeApiTests(json4s.implicitJsonAst, json4s.implicitJsonStringParser)

  object SprayTests extends JsonJava8TimeApiTests(spray.implicitJsonAst, spray.implicitJsonStringParser)

  object JacksonTests extends JsonJava8TimeApiTests(jackson.implicitJsonAst, jackson.implicitJsonStringParser)

  object ArgonautTests extends JsonJava8TimeApiTests(argonaut.implicitJsonAst, argonaut.implicitJsonStringParser)

  object CirceTests extends JsonJava8TimeApiTests(circe.implicitJsonAst, circe.implicitJsonStringParser)

  object LiftTests extends JsonJava8TimeApiTests(lift.implicitJsonAst, lift.implicitJsonStringParser)

}



abstract class JsonJava8TimeApiTests(ast: JsonAst, parser: Parser[String, JsonAst]) extends TestSuite {
  implicit def implicitAst: JsonAst = ast

  implicit def implicitParser: Parser[String, JsonAst] = parser


  case class TestLocalDate(time: LocalDate)
  case class TestLocalDateTime(time: LocalDateTime)
  case class TestLocalTime(time: LocalTime)
  case class TestZonedDateTime(time: ZonedDateTime)
  case class TestOffsetDateTime(time: OffsetDateTime)
  case class TestOffsetTime(time: OffsetTime)

  val `[Java 8 Time Extractor] Extract LocalDate` = test {
    json"""{"time":  "2017-01-23"}""".time.as[LocalDate]
  } returns LocalDate.of(2017, 1, 23)

  val `[Java 8 Time Extractor] Extract LocalDateTime` = test {
    json"""{"time":  "2017-01-23T18:28:51.045"}""".time.as[LocalDateTime]
  } returns LocalDateTime.of(2017, 1, 23, 18, 28, 51, 45000000)

  val `[Java 8 Time Extractor] Extract LocalTime` = test {
    json"""{"time":  "18:38:14.997"}""".time.as[LocalTime]
  } returns LocalTime.of(18, 38, 14, 997000000)


  val `[Java 8 Time Extractor] Extract ZonedDateTime` = test {
    json"""{"time":  "2017-01-23T18:41:02.086+02:00[Europe/Kiev]"}""".time.as[ZonedDateTime]
  } returns ZonedDateTime.of(2017, 1, 23, 18, 41, 2, 86000000, ZoneId.of("Europe/Kiev"))


  val `[Java 8 Time Extractor] Extract OffsetDateTime` = test {
    json"""{"time":  "2017-01-23T18:44:18.221+02:00"}""".time.as[OffsetDateTime]
  } returns OffsetDateTime.of(2017, 1, 23, 18, 44, 18, 221000000, ZoneOffset.ofHours(2))


  val `[Java 8 Time Extractor] Extract OffsetTime` = test {
    json"""{"time":  "19:03:25.325+02:00"}""".time.as[OffsetTime]
  } returns OffsetTime.of(19, 3, 25, 325000000, ZoneOffset.ofHours(2))


  val `[Java 8 Time Extractor] Extract Failure` = test {
    json"""{"time":  "19:03:25.325+02:0"}""".time.as[OffsetTime]
  } throws classOf[ParseException]


  val `[Java 8 Time Serializator] Serialize LocalDate` = test {
    Json(TestLocalDate(LocalDate.of(2017, 1, 23)))
  } returns json"""{"time":  "2017-01-23"}"""

  val `[Java 8 Time Serializator] Serialize LocalDateTime` = test {
    Json(TestLocalDateTime( LocalDateTime.of(2017, 1, 23, 18, 28, 51, 45000000)))
  } returns json"""{"time":  "2017-01-23T18:28:51.045"}"""


  val `[Java 8 Time Serializator] Serialize LocalTime` = test {
    Json(TestLocalTime(LocalTime.of(18, 38, 14, 997000000)))
  } returns json"""{"time":  "18:38:14.997"}"""


  val `[Java 8 Time Serializator] Serialize ZonedDateTime` = test {
    Json(TestZonedDateTime(ZonedDateTime.of(2017, 1, 23, 18, 41, 2, 86000000, ZoneId.of("Europe/Kiev"))))
  } returns json"""{"time":  "2017-01-23T18:41:02.086+02:00[Europe/Kiev]"}"""


  val `[Java 8 Time Serializator] Serialize OffsetDateTime` = test {
    Json(TestOffsetDateTime(OffsetDateTime.of(2017, 1, 23, 18, 44, 18, 221000000, ZoneOffset.ofHours(2))))
  } returns json"""{"time":  "2017-01-23T18:44:18.221+02:00"}"""


  val `[Java 8 Time Serializator] Serialize OffsetTime` = test {
    Json(TestOffsetTime(OffsetTime.of(19, 3, 25, 325000000, ZoneOffset.ofHours(2))))
  } returns json"""{"time":  "19:03:25.325+02:00"}"""

}
