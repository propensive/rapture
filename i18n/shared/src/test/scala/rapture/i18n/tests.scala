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

package rapture.i18n.test

import rapture.i18n._
import rapture.test._
import rapture.core._

class TestRun extends Programme {
  include(I18nTests)
}

object I18nTests extends TestSuite {

  import languages._

  val `Create basic English string` = test {
    val greetingEn: I18n[String, En] = en"Hello world"
    greetingEn[En]
  } returns "Hello world"

  val `Create basic French string` = test {
    val greetingFr: I18n[String, Fr] = fr"Bonjour le monde"
    greetingFr[Fr]
  } returns "Bonjour le monde"
  
  val `Create combined internationalized strings, getting English` = test {
    val greeting = en"Hello world" & fr"Bonjour le monde"
    greeting[En]
  } returns "Hello world"

  val `Create combined internationalized strings, getting French` = test {
    val greeting = en"Hello world" & fr"Bonjour le monde"
    greeting[Fr]
  } returns "Bonjour le monde"

  val `Check providing surplus language definitions allowed` = test {
    val greeting = en"Hello world" & fr"Bonjour le monde"
    val en: I18n[String, En] = greeting
    val fr: I18n[String, Fr] = greeting
    val both: I18n[String, En with Fr] = greeting
  } returns ()

  val `Check internationalized substitution` = test {
    val greeting = en"Hello world" & fr"Bonjour le monde"
    val msg = en"In English, we say '$greeting'"
    msg[En]
  } returns "In English, we say 'Hello world'"

  val `Check internationalized substitution (French)` = test {
    val greeting = en"Hello world" & fr"Bonjour le monde"
    val msg = fr"En français, on dit '$greeting'"
    msg[Fr]
  } returns "En français, on dit 'Bonjour le monde'"

  val `Substitute ordinary string` = test {
    val lang = "Scala"
    val msg = en"I speak $lang." & fr"Je parle $lang." & de"Ich spreche $lang."
    msg[De]
  } returns "Ich spreche Scala."


  object MyMessages extends LanguageBundle[En with Fr] {
    val greeting: IString = en"Hello world" & fr"Bonjour le monde"
  }

  val `Test message bundle` = test {
    MyMessages.greeting[En]
  } returns "Hello world"

  val `Test message bundle (French)` = test {
    MyMessages.greeting[Fr]
  } returns "Bonjour le monde"

  val `Use default language` = test {
    import languages.fr._
    val str: String = MyMessages.greeting
    str
  } returns "Bonjour le monde"

  val `Adding existing language does not compile` = test {
    typeMismatch {
      import deferTypeErrors._
      en"a" & fr"b" & en"c"
    }
  } returns true

  val `Adding existing language does not compile (2)` = test {
    typeMismatch {
      import deferTypeErrors._
      (en"a" & fr"b") & (de"c" & en"d")
    }
  } returns true

  val `Get language string using runtime value` = test {
    val msg = en"Hello" & fr"Bonjour"
    val langs = en | fr
    implicit val loc = langs.parse("FR")
    msg: String
  } returns "Bonjour"

  val `Get language string from subset using runtime value` = test {
    typeMismatch {
      import deferTypeErrors._
      val msg = en"Hello" & fr"Bonjour"
      val langs = en | fr | de
      implicit val loc = langs.parse("FR")
      msg: String
    }
  } returns true

  val `Get language string from superset using runtime value` = test {
    val msg = en"Hello" & fr"Bonjour" & de"Hallo"
    val langs = en | fr
    implicit val loc = langs.parse("FR")
    msg: String
  } returns "Bonjour"

}
