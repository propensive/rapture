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
import rapture.core._
import rapture.io._
import rapture.uri._

import java.text.SimpleDateFormat
import java.util.Locale

import scala.collection.mutable

case class Cookie[I, D](domain: String, name: String, value: String,
    path: RootedPath, expiry: Option[I], secure: Boolean)(implicit ts: TimeSystem[I, D]) {
  lazy val pathString = path.toString
}

class Browser[I: TimeSystem.ByInstant]() {
  val browserString = "Rapture Browser 2.0.0"
  private val Rfc1036Pattern = "EEE, dd-MMM-yyyy HH:mm:ss zzz"

  val ts = ?[TimeSystem.ByInstant[I]]

  val cookies: mutable.HashMap[(String, String, RootedPath), Cookie[I, _]] =
    new mutable.HashMap[(String, String, RootedPath), Cookie[I, _]]

  def parseCookie(s: String, domain: String): Cookie[I, _] = {
    val ps = s.split(";").map(_.trim.split("=")) map { a =>
      a(0) -> (if(a.length > 1) a(1).urlDecode else "")
    }
    val details = ps.tail.toMap

    Cookie(details.getOrElse("domain", domain), ps.head._1, ps.head._2,
      RootedPath.parse(details.getOrElse("path", "")).getOrElse(RootedPath(Vector())),
      details.get("expires") map { exp =>
        ts.instant(new SimpleDateFormat(Rfc1036Pattern, Locale.US).parse(exp).getTime)
      }, details.contains("secure"))
  }

  def domainCookies(domain: String, secure: Boolean, path: String): String = {
    val now = System.currentTimeMillis
    cookies foreach { c =>
      if(c._2.expiry.exists(e => ts.fromInstant(e) < now))
        cookies.remove((c._2.domain, c._2.name, c._2.path))
    }

    cookies.toList.filter(secure || !_._2.secure).filter(domain endsWith
        _._2.domain).filter(path startsWith _._2.pathString).map(_._2).groupBy(_.name) map { c =>
        c._1+"="+c._2.maxBy(_.pathString.length).value.urlEncode } mkString "; "
  }

  def accept[I2, D](c: Cookie[I2, D]): Boolean = c.domain.split("\\.").length > 1

  /*class BrowserUrl(url: HttpUrl) {

    def httpGet[D]()(implicit httpTimeout: HttpTimeout, httpCertificateConfig: HttpCertificateConfig,
        httpRedirectConfig: HttpRedirectConfig) = url.httpGet()
    
    def httpPost[C: PostType, D](
      content: C,
      headers: Map[String, String] = Map())
    (implicit mode: Mode[`BrowserUrl#httpPost`], httpTimeout: HttpTimeout, httpCertificateConfig: HttpCertificateConfig, httpRedirectConfig: HttpRedirectConfig, httpBasicAuthentication: HttpBasicAuthentication): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
      mode.wrap {
        implicit val m = mode.generic
        // FIXME: Reimplement this with recursion
        var u = url
        var retries = 0
        var response: HttpResponse = null

        do {
          response = mode.unwrap(u.httpPost[C, D](content, headers + ("Cookie" -> domainCookies(u.hostname, u.ssl, u.pathString))))

          val newCookies = response.headers.get("Set-Cookie").getOrElse(Nil) map { c =>
            parseCookie(c, u.hostname)
          } filter { c: Cookie[I, _] => accept(c) }
        
          for(c <- newCookies) cookies((c.domain, c.name, c.path)) = c

          if(response.status/100 == 3) {
            retries += 1
            if(retries > 5) throw TooManyRedirects()
            val dest = response.headers("Location").headOption.getOrElse(throw BadHttpResponse())

            u = if(dest.startsWith("http")) Http.parse(dest)
                else if(dest.startsWith("/")) Http / u.hostname / RootedPath.parse(dest)
                // FIXME: This doesn't handle ascent in relative paths
                else u / RootedPath.parse(dest)
          }
        } while(response.status/100 == 3)
        
        response
    }
  }

  def apply(url: HttpUrl): BrowserUrl = new BrowserUrl(url)*/
}

