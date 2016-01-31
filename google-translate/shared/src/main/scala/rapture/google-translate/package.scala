/******************************************************************************************************************\
* Rapture I18N / Google Translate, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                  *
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
package rapture.i18n.googleTranslate

import rapture.base._
import rapture.i18n._

import scala.reflect._

import language.implicitConversions
import language.experimental.macros

object NotString {
  implicit def ambiguous1: NotString[String] = null
  implicit def ambiguous2: NotString[String] = null
  implicit def unambiguous[S <: String]: NotString[S] = null
}

class NotString[S <: String]

class GoogleApiKey[S <: String](key: S)(implicit notString: NotString[S])

object GoogleApiKey {
  def apply[S <: String](key: S)(implicit notString: NotString[key.type]): GoogleApiKey[key.type] =
    new GoogleApiKey[key.type](key)(notString)
}

object `package` {
  implicit def upcast[ToLang <: Language, FromLang <: Language, S <: String](from: I18n[String, FromLang])(implicit license: GoogleApiKey[S]): I18n[String, ToLang] =
    macro Macros.missingTranslationsMacro[ToLang, FromLang, S]
}

object Macros {
  
  def missingTranslationsMacro[ToLang <: Language: c.WeakTypeTag, FromLang <: Language: c.WeakTypeTag, S <: String: c.WeakTypeTag](c:
      BlackboxContext)(from: c.Expr[I18n[String, FromLang]])(license: c.Expr[GoogleApiKey[S]]): c.Expr[I18n[String, ToLang]] = {
    
    import c.universe._
    import compatibility._

    // FIXME: Extract this correctly!
    val key = weakTypeOf[S].toString.split("\"")(1)

    val fromLangs = (normalize(c)(weakTypeOf[FromLang]) match {
      case rt: RefinedType => rt.parents
      case typ: Type => List(typ)
    }).map(_.toString.split("\\.").last.toLowerCase).to[Set]

    val toLangs = (normalize(c)(weakTypeOf[ToLang]) match {
      case rt: RefinedType => rt.parents
      case typ: Type => List(typ)
    }).map(_.toString.split("\\.").last.toLowerCase).to[Set]

    val missing = toLangs -- fromLangs

    def langs(tree: Tree): Map[String, String] = tree match {
      case Apply(Select(Apply(Select(Select(Select(id1, id2), id3), id4), List(Apply(_, List(Literal(Constant(
          str: String)))))), lang), _) if id1.toString == "rapture" && id2.toString == "i18n" &&
          id3.toString == "package" && id4.toString == "I18nEnrichedStringContext" =>
        
        Map(lang.toString -> str)
      
      case Apply(app: Apply, _) =>
        langs(app)
      
      case Apply(TypeApply(Select(q, id6), _), List(app)) if id6.toString == "$bar" =>
        langs(q) ++ langs(app)
    }

    val ls = langs(from.tree)
    val extras = ls.find(_._1 == "en").orElse(ls.find(_._1 == "fr")).getOrElse(ls.head) match {
      case (lang, text) =>
        missing.map { to =>
          val tran = GoogleTranslate.translate(key, text, lang.toUpperCase, to.toUpperCase).replaceAll("\"", "\\\\\"")
	  s"""${to.toLowerCase}"${tran}""""
        }
    }

    c.abort(c.enclosingPosition, s"""Not all required language translations have been provided. Consider appending the following translations to the expression:\n\n  ${extras.mkString("& ", " & ", "")}\n""")


  }
}

object GoogleTranslate {
  import rapture.uri._
  import rapture.net._
  import rapture.io._
  import rapture.codec._, encodings.`UTF-8`._
  import rapture.json._, jsonBackends.jawn._

  def translate(key: String, text: String, from: String, to: String): String = {
    val out = uri"https://www.googleapis.com/language/translate/v2?q=${text}&target=${to}&format=text&source=${from}&key=${key}".slurp[Char]
    Json.parse(out).data.translations(0).translatedText.as[String]
  }
}
