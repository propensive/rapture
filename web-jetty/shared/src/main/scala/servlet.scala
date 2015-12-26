/******************************************************************************************************************\
* Rapture Web, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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
package rapture.web

import rapture.core._
import rapture.mime._
import rapture.codec._
import rapture.io._
import rapture.net.{Cookie => _, _}

import scala.collection.mutable.{Map => _, _}

import javax.servlet.http._
import java.nio.ByteBuffer
import java.io._

class JettyHttpRequest(req: HttpServletRequest, resp: HttpServletResponse) extends HttpRequest {
  def contentType = {
    val ct = Option(req.getContentType) getOrElse "application/x-www-form-urlencoded"
    val ct2 = ct.split(";").head
    MimeTypes.unapply(ct2).getOrElse(MimeTypes.MimeType(ct2))
  }

  def contentLength = req.getContentLength
  def queryString = req.getQueryString
  def requestMethod = HttpMethods.method(req.getMethod)
  def scriptName = req.getRequestURI
  def https = req.isSecure
  def serverName = req.getServerName
  def serverPort = req.getServerPort
  lazy val url = req.getRequestURL.toString
  def basePathString = req.getContextPath
  def servicePathString = req.getServletPath
  def remainderString = Option(req.getPathInfo).getOrElse("")
  
  def uploadSizeLimit = 50*1024*1024
  private var uploadsValue: Map[String, Array[Byte]] = Map[String, Array[Byte]]()

  def fileUploads: Map[String, Array[Byte]] = uploadsValue

  private def stripQuotes(s: String) = if(s.startsWith("\"")) s.substring(1, s.length - 1) else s

  val parameters = contentType match {
    case MimeTypes.`multipart/form-data` =>
      val params = new HashMap[String, String]
      val ct = headers("Content-Type").head.split("; *")
      val boundary = ct.find(_.startsWith("boundary=")).get.substring(9)
      val mpr = new MultipartReader(boundary, req.getInputStream, uploadSizeLimit)
      while(mpr.ready()) {
        mpr.read() foreach { m =>
          if(m.filename.isDefined) {
            uploadsValue += stripQuotes(m.name.get) -> m.data
            params(stripQuotes(m.name.get)) = stripQuotes(m.filename.get)
          } else params(stripQuotes(m.name.get)) = new String(m.data, Option(req.getCharacterEncoding).getOrElse("ASCII"))
        }
      }
      params.toMap
    
    case MimeTypes.`application/x-www-form-urlencoded` =>
      val params = new HashMap[String, String]
      val enum = req.getParameterNames
      while(enum.hasMoreElements) {
        val name = enum.nextElement.asInstanceOf[String]
        for(value <- req.getParameterValues(name))
          params(name) = value
      }
      params.toMap
  }

  private val input = {
    val in = req.getInputStream
    import encodings.`UTF-8`._
    InputBuilder.inputStreamCharBuilder.input(in)
  }

  lazy val body =
    if(streamRead) throw new Exception("Stream has already been read")
    else {
      streamRead = true
      input.slurp()
    }

  lazy val headers = new scala.collection.immutable.HashMap[String, Seq[String]] {
    private def enum2list(e: java.util.Enumeration[_]) = {
      val lb = new ListBuffer[String]
      while(e.hasMoreElements) lb += e.nextElement.asInstanceOf[String]
      lb
    }
    override def get(key: String) = Some(enum2list(req.getHeaders(key)))
    override def size = enum2list(req.getHeaderNames).length
    override def iterator = enum2list(req.getHeaderNames).map(n => (n, enum2list(req.getHeaders(n)))).iterator
  }
}

abstract class ServletWrapper extends HttpServlet { wrapper =>
  def handle(req: HttpRequest): Response

  override def service(req: HttpServletRequest, resp: HttpServletResponse) = {
    val t0 = System.currentTimeMillis
    val vReq = try {
      new JettyHttpRequest(req, resp)
    } catch { case e: Exception =>
      throw e
    }

    val vResp = handle(vReq)

    vReq.completionTasks.foreach(_())
    
    resp.setStatus(vResp.code)
    for((n, v) <- vResp.headers) resp.addHeader(n, v)
    for(rc <- vReq.responseCookies) {
      val c = new Cookie(rc._1, rc._2)
      c.setDomain(rc._3)
      c.setMaxAge(rc._5 match {
        case Some(t) => (t/1000).toInt
        case None => -1
      })
      c.setPath(rc._4)
      c.setSecure(rc._6)
      resp.addCookie(c)
    }

    val r = try {
      vResp match {
        case BufferResponse(_, _, ct, buffers) =>
          resp.setContentType(ct.name)
          val out = resp.getOutputStream
          var spare: ByteBuffer = null
          for(b <- buffers) {
            if(b.hasArray) out.write(b.array, b.arrayOffset, b.limit)
            else {
              if(spare == null) spare = ByteBuffer.allocate(65536)
              while(b.hasRemaining) {
                spare.clear()
                spare.put(b)
                out.write(spare.array, 0, spare.position)
              }
            }
          }
          out.flush()
        
        case sr@StreamResponse(_, _, ct, send) =>
          val enc = encodings.`UTF-8`
          resp.setContentType(ct.name+"; charset="+enc.name)
          val w = new BufferedWriter(new OutputStreamWriter(resp.getOutputStream(), enc.name))
          ensuring(new CharOutput(w))(send)(_.close())
        
        case sr@ByteStreamResponse(_, _, ct, send) =>
          resp.setContentType(ct.name)
          val w = resp.getOutputStream()
          ensuring(new ByteOutput(w))(send)(_.close())
        
        case ErrorResponse(code, _, message, _) =>
          resp.sendError(code, message)
        
        case FileResponse(_, _, ct, file) =>
          resp.setContentType(ct.name)
          resp.setContentLength(file.length.toInt)
          val out = new ByteOutput(resp.getOutputStream)
          file > out
          out.flush()
        
        case RedirectResponse(_, location) =>
          resp.sendRedirect(location)
      }
    } catch { case e: Exception =>
      throw e
    } finally {
    }
    r
  }
}
