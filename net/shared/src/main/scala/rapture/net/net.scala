/******************************************************************************************************************\
* Rapture Net, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                      *
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
package rapture.net

import rapture.io._
import rapture.uri._
import rapture.mime._
import rapture.core._
import rapture.codec._

import language.existentials

import java.io._
import java.net._

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

trait PostType[-C] {
  def contentType: Option[MimeTypes.MimeType]
  def sender(content: C): Input[Byte]
}

/** Common methods for `HttpUrl`s */
trait NetUrl extends Url[NetUrl] with Uri {
  
  import javax.net.ssl._
  
  private[rapture] def javaConnection: HttpURLConnection =
    new URL(toString).openConnection().asInstanceOf[HttpURLConnection]
  
  private val trustAllCertificates = {
    Array[TrustManager](new X509TrustManager {
      override def getAcceptedIssuers(): Array[java.security.cert.X509Certificate] = null
      
      def checkClientTrusted(certs: Array[java.security.cert.X509Certificate], authType: String):
          Unit = ()
      
      def checkServerTrusted(certs: Array[java.security.cert.X509Certificate], authType: String):
          Unit = ()
    })
  }
  
  private val sslContext = SSLContext.getInstance("SSL")
  sslContext.init(null, trustAllCertificates, new java.security.SecureRandom())

  private val allHostsValid = new HostnameVerifier {
    def verify(hostname: String, session: SSLSession) = true
  }

  def hostname: String
  def port: Int
  def ssl: Boolean
  def canonicalPort: Int

  trait Base64Padded extends CodecType
  implicit val base64: ByteCodec[Base64Padded] = new Base64Codec[Base64Padded](endPadding = true)

  def schemeSpecificPart = "//"+hostname+(if(port == canonicalPort) "" else ":"+port)+pathString

  /** Sends an HTTP put to this URL.
    *
    * @param content the content to put to the URL
    * @return the HTTP response from the remote host */
  def httpPut[C: PostType, T](content: C, headers: Map[String, String] = Map())(
    implicit mode: Mode[`NetUrl#httpPut`],
    httpTimeout: HttpTimeout,
    httpRedirectConfig: HttpRedirectConfig,
    httpCertificateConfig: HttpCertificateConfig,
    httpBasicAuthentication: HttpBasicAuthentication): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] = {
      implicit val m = mode.generic
      httpPost(content, headers, "PUT")
    }

  def httpHead[T](
    headers: Map[String, String] = Map())
  (implicit mode: Mode[`NetUrl#httpHead`], httpTimeout: HttpTimeout, httpRedirectConfig: HttpRedirectConfig, httpCertificateConfig: HttpCertificateConfig, httpBasicAuthentication: HttpBasicAuthentication): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] = {
    implicit val m = mode.generic
    httpPost(None, headers, "HEAD")
  }

  def httpGet[T](
    headers: Map[String, String] = Map())
  (implicit mode: Mode[`NetUrl#httpGet`], httpTimeout: HttpTimeout, httpRedirectConfig: HttpRedirectConfig, httpCertificateConfig: HttpCertificateConfig, httpBasicAuthentication: HttpBasicAuthentication): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] = {
    implicit val m = mode.generic
    httpPost(None, headers, "GET")
  }

  /** Sends an HTTP post to this URL.
    *
    * @param content the content to post to the URL
    * @return the HTTP response from the remote host */
  def httpPost[C: PostType, T](content: C, headers: Map[String, String] = Map(), method: String = "POST")
      (implicit mode: Mode[`NetUrl#httpPost`], httpTimeout: HttpTimeout, httpRedirectConfig: HttpRedirectConfig,
      httpCertificateConfig: HttpCertificateConfig, httpBasicAuthentication: HttpBasicAuthentication): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
    mode wrap {
      // FIXME: This will produce a race condition if creating multiple URL connections with
      // different values for followRedirects in parallel
      HttpURLConnection.setFollowRedirects(httpRedirectConfig.follow)
      val conn: URLConnection = new URL(toString).openConnection()
      conn.setConnectTimeout(httpTimeout.duration)
      conn match {
        case c: HttpsURLConnection =>
          if(httpCertificateConfig.ignoreIfInvalid) {
            c.setSSLSocketFactory(sslContext.getSocketFactory)
            c.setHostnameVerifier(allHostsValid)
          }
          c.setRequestMethod(method)
          if(content != None) c.setDoOutput(true)
          c.setUseCaches(false)
        case c: HttpURLConnection =>
          c.setRequestMethod(method)
          if(content != None) c.setDoOutput(true)
          c.setUseCaches(false)
      }

      // FIXME: What an ugly way of writing this.
      httpBasicAuthentication.credentials foreach { case (username, password) =>
        conn.setRequestProperty("Authorization",
          "Basic "+base64.encode(s"$username:$password".getBytes("UTF-8")).mkString)
      }
      
      ?[PostType[C]].contentType map { ct => conn.setRequestProperty("Content-Type", ct.name) }
      for((k, v) <- headers) conn.setRequestProperty(k, v)

      if(content != None)
        ensuring(OutputStreamBuilder.output(conn.getOutputStream)) { out =>
          ?[PostType[C]].sender(content) > out
        } (_.close())

      import scala.collection.JavaConversions._

      val statusCode = conn match {
        case c: HttpsURLConnection => c.getResponseCode()
        case c: HttpURLConnection => c.getResponseCode()
      }
      
      val is = try conn.getInputStream() catch {
        case e: IOException => conn match {
          case c: HttpsURLConnection => c.getErrorStream()
          case c: HttpURLConnection => c.getErrorStream()
        }
      }
      
      new HttpResponse(mapAsScalaMap(conn.getHeaderFields()).toMap.mapValues(_.to[List]),
          statusCode, is)
    }
}

class HttpQueryParametersBase[U, T <: Iterable[U]] extends QueryType[Path[_], T] {
  def extras(existing: AfterPath, q: T): AfterPath =
    existing + ('?' -> ((q.map({ case (k: Symbol, v: String) =>
      k.name.urlEncode+"="+v.urlEncode
    }).mkString("&")) -> 1.0))
}

object HttpUrl {
  implicit val parser: StringParser[HttpUrl] = new StringParser[HttpUrl] {
    type Throws = ParseException
    def parse(s: String, mode: Mode[_]): mode.Wrap[HttpUrl, Throws] = mode.wrap(Http.parse(s))
  }

  implicit val serializer: StringSerializer[HttpUrl] = new StringSerializer[HttpUrl] {
    def serialize(h: HttpUrl): String = h.toString
  }
}

/** Represets a URL with the http scheme */
class HttpUrl(val pathRoot: NetPathRoot[HttpUrl], elements: Seq[String], afterPath: AfterPath,
    val ssl: Boolean) extends Url[HttpUrl](elements, afterPath) with NetUrl with
    PathUrl[HttpUrl] { thisHttpUrl =>
  
  def makePath(ascent: Int, xs: Seq[String], afterPath: AfterPath) =
    new HttpUrl(pathRoot, elements, afterPath, ssl)
  
  def hostname = pathRoot.hostname
  def port = pathRoot.port
  def canonicalPort = if(ssl) 443 else 80

  override def equals(that: Any) = that match {
    case that: HttpUrl =>
      pathRoot == that.pathRoot && ssl == that.ssl && afterPath == that.afterPath &&
          elements == that.elements
    case _ => false
  }

  override def hashCode =
    pathRoot.hashCode ^ elements.to[List].hashCode ^ afterPath.hashCode ^ ssl.hashCode
}

trait NetPathRoot[+T <: Url[T] with NetUrl] extends PathRoot[T] {
  def hostname: String
  def port: Int

  override def equals(that: Any) = that match {
    case that: NetPathRoot[_] => hostname == that.hostname && port == that.port
    case _ => false
  }

  override def hashCode = hostname.hashCode ^ port
}

class HttpPathRoot(val hostname: String, val port: Int, val ssl: Boolean) extends
    NetPathRoot[HttpUrl] with Uri { thisPathRoot =>
  
  def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath): HttpUrl =
    new HttpUrl(thisPathRoot, elements, Map(), ssl)

  def scheme = if(ssl) Https else Http
  def canonicalPort = if(ssl) 443 else 80
  def schemeSpecificPart = "//"+hostname+(if(port == canonicalPort) "" else ":"+port)+pathString

  override def /(element: String) = makePath(0, Array(element), Map())
  
  def /[P <: Path[P]](path: P) = makePath(0, path.elements, Map())
  
  override def equals(that: Any): Boolean =
    that.isInstanceOf[HttpPathRoot] && hostname == that.asInstanceOf[HttpPathRoot].hostname
}

/** Factory for creating new HTTP URLs */
object Http extends Scheme[HttpUrl] {
  def schemeName = "http"

  /** Creates a new URL with the http scheme with the specified domain name and port
    *
    * @param hostname A `String` of the domain name for the URL
    * @param port The port to connect to this URL on, defaulting to port 80 */
  def /(hostname: String, port: Int = services.tcp.http.portNo) =
    new HttpPathRoot(hostname, port, false)

  private val UrlRegex =
    """(https?):\/\/([\.\-a-z0-9]+)(:[1-9][0-9]*)?(\/?([^\?]*)(\?([^\?]*))?)""".r

  /** Parses a URL string into an HttpUrl */
  def parse(s: String): HttpUrl = s match {
    case UrlRegex(scheme, server, port, _, path, _, after) =>
      val rp = new SimplePath(path.split("/"), Map())
      val afterPath = after match {
        case null | "" => Map[Symbol, String]()
        case after => after.split("&").map { p => p.split("=", 2) match {
          case Array(k, v) => Symbol(k) -> v
        } }.toMap
      }
      val most = scheme match {
        case "http" =>
          Http./(server, if(port == null) 80 else port.substring(1).toInt) / rp
        case "https" =>
          Https./(server, if(port == null) 443 else port.substring(1).toInt) / rp
        case _ => throw new Exception(s)
      }
      if(afterPath.isEmpty) most else most.query(afterPath)
    case _ => throw new Exception(s)
  }
}

/** Factory for creating new HTTPS URLs */
object Https extends Scheme[HttpUrl] {
  def schemeName = "https"

  /** Creates a new URL with the https scheme with the specified domain name and port
    *
    * @param hostname A `String` of the domain name for the URL
    * @param port The port to connect to this URL on, defaulting to port 443 */
  def /(hostname: String, port: Int = services.tcp.https.portNo) =
    new HttpPathRoot(hostname, port, true)
  
  def parse(s: String): HttpUrl = Http.parse(s)
}
