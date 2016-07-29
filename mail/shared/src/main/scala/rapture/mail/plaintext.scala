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
        val output = sh"elinks -dump -dump-width=76 'file://$file'".exec[String]
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
