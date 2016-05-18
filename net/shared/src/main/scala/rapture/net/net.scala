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
import rapture.mime._
import rapture.core._
import rapture.codec._

import java.io._
import javax.net.ssl._

case class TimeoutException() extends Exception("Timeout")
case class InvalidCertificateException() extends Exception("Timeout")

object HttpTimeout {
  implicit val defaultHttpTimeout: HttpTimeout { type Throws = TimeoutException } =
    new HttpTimeout(10000) {
      type Throws = TimeoutException
    }

  def apply[T: TimeSystem.ByDuration](timeout: T) =
    new HttpTimeout(Math.min(Int.MaxValue, ?[TimeSystem.ByDuration[T]].fromDuration(timeout)).toInt) {
      type Throws = TimeoutException
    }
}
class HttpTimeout(val duration: Int) {
  type Throws <: Exception
}

class HttpCertificateConfig(val ignoreIfInvalid: Boolean) {
  type Throws <: Exception
}

object HttpCertificateConfig {
  implicit val defaultHttpCertificateConfig: HttpCertificateConfig { type Throws = InvalidCertificateException } =
    new HttpCertificateConfig(true) { type Throws = InvalidCertificateException }
}
object HttpRedirectConfig {
  implicit val defaultHttpRedirectConfig: HttpRedirectConfig = new HttpRedirectConfig(true)
}
class HttpRedirectConfig(val follow: Boolean)

object HttpBasicAuthentication {
  implicit val defaultHttpBasicAuthentication: HttpBasicAuthentication = new HttpBasicAuthentication(None)
}
class HttpBasicAuthentication(val credentials: Option[(String, String)]) {
  type Throws
}

object httpOptions {
  object noTimeout {
    implicit val implicitHttpTimeout: HttpTimeout { type Throws = Nothing } = new HttpTimeout(-1) {
      type Throws = Nothing
    }
  }
  
  object ignoreInvalidCertificates {
    implicit val implicitCertificateConfig: HttpCertificateConfig = new HttpCertificateConfig(true)
  }
  
  object doNotFollowRedirects {
    implicit val implicitFollowRedirects: HttpRedirectConfig = new HttpRedirectConfig(false)
  }
}

object HttpMethods {
  
  private val methods = new scala.collection.mutable.HashMap[String, Method]
  
  sealed class Method(val string: String) {
    
    def unapply(r: String) = r == string
    override def toString = string
    
    methods += string -> this
  }

  trait FormMethod { this: Method => }

  def method(s: String) = methods(s)

  val Get = new Method("GET") with FormMethod
  val Put = new Method("PUT")
  val Post = new Method("POST") with FormMethod
  val Delete = new Method("DELETE")
  val Trace = new Method("TRACE")
  val Options = new Method("OPTIONS")
  val Head = new Method("HEAD")
  val Connect = new Method("CONNECT")
  val Patch = new Method("PATCH")

}

class HttpResponse(val headers: Map[String, List[String]], val status: Int, is: InputStream) {
  def input[Data](implicit ib: InputBuilder[InputStream, Data], mode: Mode[`HttpResponse#input`]):
      mode.Wrap[Input[Data], Exception]=
    mode.wrap(ib.input(is))
}

object PostType {
  implicit val nonePostType: PostType[None.type] = new PostType[None.type] {
    def contentType = Some(MimeTypes.`application/x-www-form-urlencoded`)
    def sender(content: None.type) = ByteArrayInput(Array[Byte](0))
  }
  
  implicit def formPostType: PostType[Map[Symbol, String]] = new PostType[Map[Symbol, String]] {
    def contentType = Some(MimeTypes.`application/x-www-form-urlencoded`)
    def sender(content: Map[Symbol, String]) = ByteArrayInput((content map { case (k, v) =>
      java.net.URLEncoder.encode(k.name, "UTF-8")+"="+java.net.URLEncoder.encode(v, "UTF-8")
    } mkString "&").getBytes("UTF-8"))
  }
  
  implicit def stringPostType[S: StringSerializer](implicit enc: Encoding): PostType[S] = new PostType[S] {
    def contentType = Some(MimeTypes.`text/plain`)
    def sender(content: S) = implicitly[StringSerializer[S]].serialize(content).input[Byte]
  }
}
trait PostType[-C] {
  def contentType: Option[MimeTypes.MimeType]
  def sender(content: C): Input[Byte]
}

object NetUrl {
  val sslContext = SSLContext.getInstance("SSL")
  val allHostsValid = new HostnameVerifier {
    def verify(hostname: String, session: SSLSession) = true
  }

  trait Base64Padded extends CodecType
  implicit val base64: ByteCodec[Base64Padded] = new Base64Codec[Base64Padded](endPadding = true)

}

trait NetUrl {
  
  private val trustAllCertificates = {
    Array[TrustManager](new X509TrustManager {
      override def getAcceptedIssuers(): Array[java.security.cert.X509Certificate] = null
      
      def checkClientTrusted(certs: Array[java.security.cert.X509Certificate], authType: String):
          Unit = ()
      
      def checkServerTrusted(certs: Array[java.security.cert.X509Certificate], authType: String):
          Unit = ()
    })
  }
  
  NetUrl.sslContext.init(null, trustAllCertificates, new java.security.SecureRandom())

  def hostname: String
  def port: Int
  def ssl: Boolean
  def canonicalPort: Int
}

object HttpUrl {
  implicit val parser: StringParser[HttpUrl] = new StringParser[HttpUrl] {
    type Throws = ParseException
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[HttpUrl, Throws] = mode.wrap(Http.parse(s))
  }

  implicit val serializer: StringSerializer[HttpUrl] = new StringSerializer[HttpUrl] {
    def serialize(h: HttpUrl): String = h.toString
  }
  
  implicit def uriCapable: UriCapable[HttpUrl] = new UriCapable[HttpUrl] {
    def uri(cp: HttpUrl) = {
      val portString = if(cp.ssl && cp.port == 443 || !cp.ssl && cp.port == 80) "" else s":${cp.port}"
      Uri(if(cp.ssl) "https" else "http", s"//${cp.hostname}${portString}/${cp.elements.mkString("/")}")
    }
  }

  implicit def urlSlashRootedPath[RP <: RootedPath]: Dereferenceable[HttpUrl, RP, HttpUrl] =
    new Dereferenceable[HttpUrl, RP, HttpUrl] {
      def dereference(p1: HttpUrl, p2: RP) = {
        val start = if(p1.elements.lastOption == Some("")) p1.elements.init else p1.elements
	HttpUrl(p1.root, start ++ p2.elements)
      }
    }

  implicit def urlSlashRelativePath[RP <: RelativePath]: Dereferenceable[HttpUrl, RP, HttpUrl] =
    new Dereferenceable[HttpUrl, RP, HttpUrl] {
      def dereference(p1: HttpUrl, p2: RP) =
	HttpUrl(p1.root, p1.elements.dropRight(p2.ascent) ++ p2.elements)
    }

  implicit def urlSlashString: Dereferenceable[HttpUrl, String, HttpUrl] =
    new Dereferenceable[HttpUrl, String, HttpUrl] {
      def dereference(p1: HttpUrl, p2: String) = {
        val start = if(p1.elements.lastOption == Some("")) p1.elements.init else p1.elements
        HttpUrl(p1.root, start :+ p2)
      }
    }

  implicit def urlParentable: Parentable[HttpUrl, HttpUrl] = new Parentable[HttpUrl, HttpUrl] {
    def parent(httpUrl: HttpUrl): HttpUrl = HttpUrl(httpUrl.root, httpUrl.elements.dropRight(1))
  }
}

/** Represets a URL with the http scheme */
case class HttpUrl(root: HttpDomain, elements: Vector[String]) extends NetUrl {
 
  override def toString = HttpUrl.uriCapable.uri(this).toString

  def hostname = root.hostname
  def port = root.port
  def canonicalPort = if(root.ssl) 443 else 80
  def ssl = root.ssl

  def query[Q: Query](q: Q): HttpQuery = HttpQuery(this, ?[Query[Q]].queryString(q))

}

trait Query[-T] {
  def queryString(t: T): String
}

object Query {
  implicit def mapQuery[K: StringSerializer, V: StringSerializer]: Query[Map[K, V]] = new Query[Map[K, V]] {
    def queryString(m: Map[K, V]): String = m.map {
      case (k, v) =>
        val key = java.net.URLEncoder.encode(?[StringSerializer[K]].serialize(k), "UTF-8")
	val value = java.net.URLEncoder.encode(?[StringSerializer[V]].serialize(v), "UTF-8")
	s"$key=$value"
    }.mkString("&")
  }
}



object HttpQuery {
  implicit val serializer: StringSerializer[HttpQuery] = new StringSerializer[HttpQuery] {
    def serialize(h: HttpQuery): String = h.toString
  }
  
  implicit val httpUrlParser: StringParser[HttpUrl] = new StringParser[HttpUrl] {
    type Throws = ParseException
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[HttpUrl, Throws] =
      mode.wrap(Http.parse(s))
  }
  
  implicit val httpQueryParser: StringParser[HttpQuery] = new StringParser[HttpQuery] {
    type Throws = ParseException
    def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[HttpQuery, Throws] =
      mode.wrap(HttpQuery.parse(s))
  }
  
  implicit def uriCapable: UriCapable[HttpQuery] = new UriCapable[HttpQuery] {
    def uri(hq: HttpQuery) = {
      val httpUrlUri = HttpUrl.uriCapable.uri(hq.httpUrl)
      Uri(httpUrlUri.scheme, s"${httpUrlUri.schemeSpecificPart}?${hq.queryString}")
    }
  }
  
  private val UrlRegex =
    """(https?):\/\/([\.\-a-z0-9]+)(:[1-9][0-9]*)?(\/?([^\?]*)(\?([^\?]*))?)""".r
  
  def parse(s: String): HttpQuery = s match {
    case UrlRegex(scheme, server, port, _, path, _, after) =>
      
      val rp = RootedPath(path.split("/").to[Vector])

      val httpUrl = scheme match {
        case "http" =>
          Http(server, if(port == null) 80 else port.substring(1).toInt) / rp
        case "https" =>
          Https(server, if(port == null) 443 else port.substring(1).toInt) / rp
        case _ => throw new Exception(s)
      }

      HttpQuery(httpUrl, Option(after).getOrElse(""))
      
    case _ => throw new Exception(s)
  }
}

case class HttpQuery(httpUrl: HttpUrl, queryString: String) {
  override def toString = {
    val httpUrlUri = HttpUrl.uriCapable.uri(httpUrl)
    s"$httpUrlUri?$queryString"
  }
}

object HttpDomain {
  implicit def cpSlashString: Dereferenceable[HttpDomain, String, HttpUrl] =
    new Dereferenceable[HttpDomain, String, HttpUrl] {
      def dereference(p1: HttpDomain, p2: String) = HttpUrl(p1, Vector(p2))
    }

  implicit def cpSlashRelativePath[RP <: RelativePath]: Dereferenceable[HttpDomain, RP, HttpUrl] =
    new Dereferenceable[HttpDomain, RP, HttpUrl] {
      def dereference(p1: HttpDomain, p2: RP) = HttpUrl(p1, p2.elements)
    }

  implicit def cpSlashRootedPath[RRP <: RootedPath]: Dereferenceable[HttpDomain, RRP, HttpUrl] =
    new Dereferenceable[HttpDomain, RRP, HttpUrl] {
      def dereference(p1: HttpDomain, p2: RRP) = HttpUrl(p1, p2.elements)
    }

  implicit def uriCapable: UriCapable[HttpDomain] = new UriCapable[HttpDomain] {
    def uri(cp: HttpDomain) = {
      val portString = if(cp.ssl && cp.port == 443 || !cp.ssl && cp.port == 80) "" else s":${cp.port}"
      Uri(if(cp.ssl) "https" else "http", s"//${cp.hostname}$portString")
    }
  }

}

case class HttpDomain(hostname: String, port: Int, ssl: Boolean) {
  override def toString = HttpDomain.uriCapable.uri(this).toString
}

object Http {

  def apply(hostname: String, port: Int = services.tcp.http.portNo) =
    HttpDomain(hostname, port, false)
  
  def parse(s: String): HttpUrl = HttpQuery.parse(s).httpUrl

}

object Https {

  def apply(hostname: String, port: Int = services.tcp.https.portNo) =
    HttpDomain(hostname, port, true)
  
  def parse(s: String): HttpUrl = HttpQuery.parse(s).httpUrl
}
