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

package rapture.net
import rapture.io._
import rapture.uri._
import rapture.core._
import rapture.base._
import rapture.codec._
import java.io.{Reader => _, Writer => _, _}

import scala.language.implicitConversions
import scala.language.experimental.macros

trait `NetUrl#httpGet` extends MethodConstraint
trait `NetUrl#httpPost` extends MethodConstraint
trait `NetUrl#httpHead` extends MethodConstraint
trait `NetUrl#httpPut` extends MethodConstraint
trait `Tcp.listen` extends MethodConstraint
trait `BrowserUrl#httpPost` extends MethodConstraint
trait `Ftp.parse` extends MethodConstraint
trait `HttpResponse#input` extends MethodConstraint
trait `Ipv6.parse` extends MethodConstraint

object `package` {

  implicit class EnrichedHttpUriContext(uc: UriContext.type) {
    def http(constants: List[String])(variables: List[String]) = macro NetMacros.httpUrlMacro
    def https(constants: List[String])(variables: List[String]) = macro NetMacros.httpsUrlMacro
  }

  implicit def httpUrlSizable(implicit httpTimeout: HttpTimeout, toUri: UriCapable[HttpUrl]): Sizable[HttpUrl, Byte] =
    new Sizable[HttpUrl, Byte] {
      type ExceptionType = HttpExceptions
      HttpSupport.basicHttpSupport
      def size(url: HttpUrl): Long = url.httpHead().headers.get("Content-Length").get.head.toLong
    }

  implicit def httpCapable[Res: HttpSupport](res: Res): HttpSupport.Capability[Res] =
    new HttpSupport.Capability[Res](res)

  implicit val httpStreamByteReader: JavaInputStreamReader[HttpUrl] = new JavaInputStreamReader[HttpUrl]({ u =>
    new java.net.URL(u.uri.toString).openConnection.asInstanceOf[java.net.HttpURLConnection].getInputStream
  })

  implicit val httpQueryStreamByteReader: JavaInputStreamReader[HttpQuery] = new JavaInputStreamReader[HttpQuery]({
    u =>
      new java.net.URL(u.uri.toString).openConnection.asInstanceOf[java.net.HttpURLConnection].getInputStream
  })

  implicit val httpResponseCharReader: Reader[HttpResponse, Char] = new Reader[HttpResponse, Char] {
    def input(response: HttpResponse): Input[Char] = {
      import encodings.`UTF-8`._
      response.input[Char]
    }
  }

  implicit val httpResponseByteReader: Reader[HttpResponse, Byte] = new Reader[HttpResponse, Byte] {
    def input(response: HttpResponse): Input[Byte] =
      response.input[Byte](?[InputBuilder[InputStream, Byte]], modes.throwExceptions())
  }

  implicit val socketStreamByteReader: JavaInputStreamReader[SocketUri] =
    new JavaInputStreamReader[SocketUri](_.javaSocket.getInputStream)

  implicit val socketStreamByteWriter: JavaOutputStreamWriter[SocketUri] =
    new JavaOutputStreamWriter[SocketUri](_.javaSocket.getOutputStream)

  implicit val socketStreamByteAppender: JavaOutputAppender[SocketUri] =
    new JavaOutputAppender[SocketUri](_.javaSocket.getOutputStream)
}

object NetMacros {

  def httpUrlMacro(c: WhiteboxContext)(constants: c.Expr[List[String]])(variables: c.Expr[List[String]]): c.Expr[Any] = {
    import c.universe._

    constants.tree match {
      case Apply(_, List(rawParts@_*)) =>
        val httpQuery = q"""_root_.rapture.net.HttpQuery.parse("http:" + $constants.zip($variables :+ "").map { case (a, b) => a + b }.mkString)"""
        if(rawParts.mkString contains "?") c.Expr(httpQuery)
        else c.Expr(q"$httpQuery.httpUrl")
    }
  }
  
  def httpsUrlMacro(c: WhiteboxContext)(constants: c.Expr[List[String]])(variables: c.Expr[List[String]]): c.Expr[Any] = {
    import c.universe._

    constants.tree match {
      case Apply(_, List(rawParts@_*)) =>
        val httpQuery = q"""_root_.rapture.net.HttpQuery.parse("https:" + $constants.zip($variables :+ "").map { case (a, b) => a + b }.mkString)"""
        if(rawParts.mkString contains "?") c.Expr(httpQuery)
        else c.Expr(q"$httpQuery.httpUrl")
    }
  }
}
