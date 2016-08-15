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

import scala.xml._

import scala.concurrent._

import rapture.io._
import rapture.core._
import rapture.time._
import rapture.json._
import rapture.uri._
import rapture.mime._
import rapture.fs._
import rapture.html._
import rapture.dom._
import rapture.codec._

trait HttpHandler_1 {
  implicit val linkRedirectHandler = new HttpHandler[PathLink] {
    def response(link: PathLink) = RedirectResponse(Nil, link.toString)
  }
}

trait HttpHandler[-T] { httpHandler =>
  def response(t: T): Response
  def contraMap[S](fn: S => T): HttpHandler[S] = new HttpHandler[S] {
    def response(t: S) = httpHandler.response(fn(t))
  }
}

object extensionBasedMimeTypes {
  implicit def handler[T: Linkable](implicit reader: Reader[T, Byte]): HttpHandler[T] = new HttpHandler[T] {
    def response(in: T) = {
      val input = reader.input(in)
      val parts = implicitly[Linkable[T]].link(in).link.split("\\.").to[List]
      val extension = if (parts.length < 2) Nil else List(parts.last)
      val mime = extension.flatMap(MimeTypes.extension).headOption.getOrElse(MimeTypes.`application/octet-stream`)
      ByteStreamResponse(200, Response.NoCache, mime, { os =>
        input > os
        os.close()
      })
    }
  }
}

object HttpHandler extends HttpHandler_1 {

  implicit def charInputHandler(implicit enc: Encoding, mimeType: MimeTypes.MimeType) = new HttpHandler[Input[Char]] {
    def response(in: Input[Char]) =
      StreamResponse(200, Response.NoCache, mimeType, { os =>
        in > os
        os.close()
      })
  }

  implicit val StringInputHandler = new HttpHandler[Input[String]] {
    import encodings.`UTF-8`._
    def response(in: Input[String]) =
      StreamResponse(200, Response.NoCache, MimeTypes.`text/plain`, { os =>
        var ln = in.read()
        while (ln != None) {
          (ln + "\n").input > os
          ln = in.read()
        }
        os.close()
      })
  }

  implicit def xmlHandler(implicit enc: Encoding) = new HttpHandler[Seq[Node]] {
    def response(t: Seq[Node]) =
      StreamResponse(200, Response.NoCache, MimeTypes.`application/xml`, { os =>
        ("<?xml version=\"1.0\" encoding=\"" + enc.name + "\"?>\n").input > os
        t.toString.input > os
        os.close()
      })
  }

  implicit def htmlDocHandler(implicit f: DomFormatter[String]): HttpHandler[HtmlDoc] = new HttpHandler[HtmlDoc] {
    def response(htmlDoc: HtmlDoc) =
      StreamResponse(200, Response.NoCache, MimeTypes.`text/html`, { out =>
        (htmlDoc.doctype.toString + "\n").input > out
        htmlDoc.html.format.input > out
        out.close()
      })(encodings.`UTF-8`())
  }

  /*implicit def csvHandler(implicit enc: Encoding) = new HttpHandler[Csv] {
    def response(csv: Csv) = StreamResponse(200, List("Pragma" -> "public",
        "Content-Disposition" -> "attachment;filename=data.csv",
        "Expires" -> "Sat, 6 May 1995 12:00:00 GMT",
        "Cache-Control" -> "no-store, no-cache, must-revalidate, post-check=0, pre-check=0"
        ),
        MimeTypes.`text/csv`, { os =>
      csv.toString.input > os
      os.close()
    })
  }*/

  /*implicit def cssHandler(implicit enc: Encoding) = new HttpHandler[HtmlCss.Stylesheet] {
    def response(css: HtmlCss.Stylesheet) = StreamResponse(200, Response.NoCache,
        MimeTypes.`text/css`, { os =>
      css.toString.input > os
      os.close()
    })
  }*/

  implicit def stringHandler(implicit enc: Encoding) = new HttpHandler[String] {
    def response(t: String) =
      StreamResponse(200, Response.NoCache, MimeTypes.`text/plain`, { os =>
        t.input > os
        os.close()
      })
  }

  implicit def byteInputHandleri(implicit mimeType: MimeTypes.MimeType) = new HttpHandler[Input[Byte]] {
    def response(in: Input[Byte]) =
      ByteStreamResponse(200, Response.NoCache, mimeType, { os =>
        in > os
        os.close()
      })
  }

  implicit def fileHandler = new HttpHandler[FsUrl] {
    def response(file: FsUrl) =
      FileResponse(200,
                   Response.NoCache,
                   file.extension.toList.flatMap(MimeTypes.extension).headOption.getOrElse(MimeTypes.`text/plain`),
                   file)
  }

  implicit def cacheHandler[T](implicit h: HttpHandler[T]): HttpHandler[Cached[T]] = new HttpHandler[Cached[T]] {
    def response(resp: Cached[T]) = {
      val r = h.response(resp.toCache)
      val dateFormat = DateFormat("EEE, d MMM yyyy")
      val timeFormat = TimeFormat("HH:mm:ss z")
      val lastModified = List("Last-modified" -> resp.lastModified.format(dateFormat, timeFormat))
      r match {
        case BufferResponse(code, headers, contentType, buffers) =>
          BufferResponse(code, lastModified, contentType, buffers)
        case sr @ StreamResponse(code, headers, contentType, send) =>
          StreamResponse(code, lastModified, contentType, send)(sr.encoding)
        case ByteStreamResponse(code, headers, contentType, send) =>
          ByteStreamResponse(code, lastModified, contentType, send)
        case ErrorResponse(code, headers, message, detail) =>
          ErrorResponse(code, lastModified, message, detail)
        case FileResponse(code, headers, contentType, file) =>
          FileResponse(code, lastModified, contentType, file)
        case RedirectResponse(headers, location) =>
          RedirectResponse(lastModified, location)
      }
    }
  }

  implicit def attachmentHandler[T](implicit h: HttpHandler[T]): HttpHandler[Attachment[T]] =
    new HttpHandler[Attachment[T]] {
      def response(resp: Attachment[T]) = {
        val r = h.response(resp.original)
        val headers = ("Content-Disposition" -> ("attachment; filename=" + resp.filename)) :: r.headers.toList
        r match {
          case BufferResponse(code, headers, contentType, buffers) =>
            BufferResponse(code, headers, contentType, buffers)
          case sr @ StreamResponse(code, headers, contentType, send) =>
            StreamResponse(code, headers, contentType, send)(sr.encoding)
          case ByteStreamResponse(code, headers, contentType, send) =>
            ByteStreamResponse(code, headers, contentType, send)
          case ErrorResponse(code, headers, message, detail) =>
            ErrorResponse(code, headers, message, detail)
          case FileResponse(code, headers, contentType, file) =>
            FileResponse(code, headers, contentType, file)
          case RedirectResponse(headers, location) =>
            RedirectResponse(headers, location)
        }
      }
    }

  implicit def futureHandler[T](implicit h: HttpHandler[T], ec: ExecutionContext): HttpHandler[Future[T]] =
    new HttpHandler[Future[T]] {
      def response(future: Future[T]) = h.response(Await.result(future, duration.Duration.Inf))
    }

  implicit val nullHandler = new HttpHandler[Response] { def response(r: Response) = r }

  implicit def jsonHandler(implicit enc: Encoding) = new HttpHandler[Json] {
    def response(t: Json) =
      StreamResponse(200, Response.NoCache, MimeTypes.`application/json`, { os =>
        t.toString.input > os
        os.close()
      })
  }

  /*implicit def pageHandler(implicit enc: Encoding) = new HttpHandler[Layout.Page] {
    def response(page: Layout.Page) = StreamResponse(page.httpStatus, Response.NoCache,
        MimeTypes.`text/html`, { os =>
      page.stream > os
    })
  }*/
}
