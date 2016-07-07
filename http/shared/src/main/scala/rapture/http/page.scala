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

package rapture.http

/*import rapture.io._
import rapture.core._
import rapture.net._
import rapture.mime._
import rapture.uri._
import rapture.html._

import htmlSyntax._

object Layout {

  import MimeTypes._

  trait PageMetadata { page: Page =>
    override def metas: List[Element[Metadata]] =
      (metaData.toList map { case (k, v) => Meta(htmlSyntax.name = k, htmlSyntax.content = v)() }) :::
          page.metas
    
    def metaData: collection.Map[Symbol, String] = collection.Map(
      'description = metaDescription,
      'keywords = metaKeywords.mkString(","),
      'author = metaAuthor
    )

    def metaDescription: String
    def metaKeywords: List[String]
    def metaAuthor: String
  }

  trait JQueryUi extends Page { this: JQuery =>
    def jQueryUiLocation = Http / "ajax.googleapis.com" / "ajax" / "libs" / "jqueryui" / "1.8.23" /
        "jquery-ui.min.js"
    
    override def scripts: List[Element[Metadata]] =
      Script(scriptType = `text/javascript`, src = PathLink(jQueryUiLocation.schemeSpecificPart)) :: super.scripts
  }

  trait JQuery extends Page {

    def jQueryLocation: HttpUrl = Http / "ajax.googleapis.com" / "ajax" / "libs" / "jquery" / "1.7.2" / "jquery.min.js"

    override def scripts: List[Element[Metadata]] =
      Script(scriptType = `text/javascript`, src = PathLink(jQueryLocation.schemeSpecificPart)) :: super.scripts
  }

  abstract class Page { page =>

    def httpStatus = 200

    def doctype = "<!DOCTYPE html>"

    def stylesheets: List[Stylesheet] = Nil
    case class Stylesheet(link: PathLink)

    def lang: String = "en"
    def title: String

    def links: List[Element[Metadata]] =
      stylesheets map { ss => Link(rel = "stylesheet", href = ss.link)() }
    
    def scripts: List[Element[Metadata]] = Nil
    def styles: List[Element[Metadata]] = Nil
    def metas: List[Element[Metadata]] = Nil

    def head =
      Title(page.title) :: styles.reverse ::: links.reverse ::: scripts.reverse ::: metas

    def body: List[Element[Flow]]

    def document =
      Html(htmlSyntax.lang = page.lang)(
        Head(page.head: _*),
        Body(page.body: _*)
      )

    def stream: Input[Char] = {
      val sb = new StringBuilder
      sb.append(doctype)
      sb.append(document.toString)
      sb.toString.input[Char]
    }
  }
  
  trait Bootstrap extends Page {
   
    def bootstrapLocation = Http / "twitter.github.com" / "bootstrap" / "1.4.0" / "bootstrap.min.css"

    override def links: List[Element[Metadata]] =
      Link(rel = "stylesheet", href = bootstrapLocation)() :: super.links

  }

  import Forms._
   
  trait TinyMce extends Page {

    def tinyMceLocation: PathLink

    override def scripts: List[Element[Metadata]] =
      Script(scriptType = `text/javascript`, src = tinyMceLocation)() :: super.links
 
  }
  
  trait TinyMceForm { this: WebForm =>

    implicit val tinyMceEditorRenderer =
      new Renderer[String, Field[String], HtmlEditor] {
        def render(f: Field[String], w: HtmlEditor) =
          Textarea(style = width(100.percent), htmlSyntax.name = f.name, cls = "mceEditorCustom")(raw(f.fieldValue))
      }
  }
  
}*/
