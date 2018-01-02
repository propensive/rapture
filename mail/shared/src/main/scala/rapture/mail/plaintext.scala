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

package rapture.mail

import rapture.core._
import rapture.io._
import rapture.fs._
import rapture.uri._
import rapture.html._
import rapture.cli._, environments.enclosing._
import rapture.codec._, encodings.`UTF-8`._

package htmlToPlainTextConverters {

  object elinks {
    implicit val htmlToPlainTextConverter: HtmlToPlainTextConverter = new HtmlToPlainTextConverter {
      def convert(html: HtmlDoc): String = {
        val file = uri"file:///tmp/${Guid.generate()()}.html"
        html.format.copyTo(file)
        val output = sh"elinks -dump -dump-width 76 '$file'".exec[String]
        file.delete()
        output
      }
    }
  }

  object links {
    implicit val htmlToPlainTextConverter: HtmlToPlainTextConverter = new HtmlToPlainTextConverter {
      def convert(html: HtmlDoc): String = {
        val file = uri"file:///tmp/${Guid.generate()()}.html"
        html.format.copyTo(file)
        val output = sh"links -dump 'file://$file'".exec[String]
        file.delete()
        output
      }
    }
  }

  object lynx {
    implicit val htmlToPlainTextConverter: HtmlToPlainTextConverter = new HtmlToPlainTextConverter {
      def convert(html: HtmlDoc): String = {
        val file = uri"file:///tmp/${Guid.generate()()}.html"
        html.format.copyTo(file)
        val output = sh"lynx -dump -width=76 'file://$file'".exec[String]
        file.delete()
        output
      }
    }
  }

}

trait HtmlToPlainTextConverter {
  def convert(html: HtmlDoc): String
}
