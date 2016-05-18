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

package rapture.i18n

import scala.reflect.ClassTag
import language.implicitConversions
import scala.annotation._
import rapture.core._
import language.experimental.macros
import language.implicitConversions

object Locale {
  implicit def upcastLocale[From <: Language, ToLang <: From](loc: Locale[From]): Locale[ToLang] =
    loc.asInstanceOf[Locale[ToLang]]
}
case class Locale[L <: Language: ClassTag]() {
  val classTag: ClassTag[L] = implicitly[ClassTag[L]]
  val name = classTag.runtimeClass.getName.split("\\.").last.toLowerCase
  def from[T, L2 <: L](i18n: I18n[T, L2]) = i18n(classTag)

  def |[L2 <: Language](locale: Locale[L2]): LocaleParser[L with L2] =
    new LocaleParser[L with L2](Map(name -> this, locale.name -> locale))

  override def toString = name

  implicit val defaultLanguage: DefaultLanguage[L] = DefaultLanguage[L](classTag)
}

case class LocaleException(locale: String) extends Exception(s"Locale '$locale' not recognised.")

class LocaleParser[L <: Language](val locales: Map[String, Locale[_ >: L <: Language]]) {
  def |[L2 <: Language](locale: Locale[L2]) = new LocaleParser[L with L2](locales + (locale.name -> locale))

  def parse(s: String)(implicit mode: Mode[_]): mode.Wrap[Locale[L], LocaleException] = mode.wrap {
    locales.get(s.toLowerCase) match {
      case Some(loc) => loc.asInstanceOf[Locale[L]]
      case None => mode.exception(LocaleException(s))
    }
  }

  override def toString = locales.keys.mkString("|")
}

object I18n {

  implicit def upcast[ToLang <: Language, FromLang <: Language](from: I18n[String, FromLang]): I18n[String, ToLang] =
    macro Macros.missingTranslationsMacro[ToLang, FromLang]

  implicit def convertToType[T, L <: Language, L2 >: L <: Language: DefaultLanguage](i18n: I18n[T, L]): T =
    i18n.map(implicitly[DefaultLanguage[L2]].tag)

  class `I18n.apply`[L <: Language]() {
    def apply[T](value: T)(implicit ct: ClassTag[L]) = new I18n[T, L](Map(ct -> value))
  }

  def apply[L <: Language] = new `I18n.apply`[L]()
}

@implicitNotFound("This I18nString already includes the language $"+"{Lang}.")
private[i18n] trait RequireLanguage[Lang]
private[i18n] object RequireLanguage { implicit def requireLanguage: RequireLanguage[Language] = null }

class I18n[T, Languages <: Language](private val map: Map[ClassTag[_], T]) {
  def apply[Lang >: Languages](implicit ct: ClassTag[Lang]): T = map(ct)

  final def &[L >: Languages <: Language, Lang2 <: L](s2: I18n[T, Lang2])(implicit ev: RequireLanguage[L]):
      I18n[T, Languages with Lang2] = new I18n[T, Languages with Lang2](map ++ s2.map)

  override def toString = {
    val langs = map.keys.map(_.runtimeClass.getName.takeRight(2).toLowerCase).mkString("&")
    val content: Option[T] = map.get(implicitly[ClassTag[En]])
    lazy val first: Option[T] = map.headOption.flatMap { case (k, v) => map.get(k) }
    val value = content.orElse(first).map(_.toString).getOrElse("") match {
      case string: String => string
      case other => s""""$other""""
    }
    s"$langs:$value"
  }

  override def hashCode = map.hashCode ^ 248327829

  override def equals(that: Any) = that match {
    case that: I18n[_, _] => map == that.map
    case _ => false
  }
}

object I18nStringParam {
  implicit def stringToStringParam[L <: Language](s: String): I18nStringParam[L] =
    new I18nStringParam[L](new I18n[String, L](Map()) {
      override def apply[Lang >: L](implicit ct: ClassTag[Lang]) = s
    })

  implicit def toI18nStringParam[L <: Language](s: I18n[String, L]): I18nStringParam[L] =
    I18nStringParam(s)
}
case class I18nStringParam[+L <: Language](i18n: I18n[String, L @annotation.unchecked.uncheckedVariance])
    extends AnyVal

trait LanguageBundle[Langs <: Language] {
  type IString = I18n[String, Langs]
}
