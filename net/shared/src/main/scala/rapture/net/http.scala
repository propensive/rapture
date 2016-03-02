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
import rapture.core._

import language.existentials

import java.io._
import java.net._
import javax.net.ssl._


object HttpSupport {
  class Capability[Res](res: Res) {
    def httpPut[C: PostType](
      content: C,
      headers: Map[String, String] = Map()
    )(
      implicit httpSupport: HttpSupport[Res],
      mode: Mode[`NetUrl#httpPut`],
      httpTimeout: HttpTimeout,
      httpRedirectConfig: HttpRedirectConfig,
      httpCertificateConfig: HttpCertificateConfig,
      httpBasicAuthentication: HttpBasicAuthentication
    ): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
      mode.wrap(httpSupport.doHttp(res, content, headers, "PUT"))
    
    def httpHead(
      headers: Map[String, String] = Map()
    )(
      implicit httpSupport: HttpSupport[Res],
      mode: Mode[`NetUrl#httpHead`],
      httpTimeout: HttpTimeout,
      httpRedirectConfig: HttpRedirectConfig,
      httpCertificateConfig: HttpCertificateConfig,
      httpBasicAuthentication: HttpBasicAuthentication
    ): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
      mode.wrap(httpSupport.doHttp(res, None, headers, "HEAD"))
      
    def httpGet(
      headers: Map[String, String] = Map()
    )(
      implicit httpSupport: HttpSupport[Res],
      mode: Mode[`NetUrl#httpHead`],
      httpTimeout: HttpTimeout,
      httpRedirectConfig: HttpRedirectConfig,
      httpCertificateConfig: HttpCertificateConfig,
      httpBasicAuthentication: HttpBasicAuthentication
    ): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
      mode.wrap(httpSupport.doHttp(res, None, headers, "GET"))
  
    def httpPost[C: PostType](
      content: C,
      headers: Map[String, String] = Map()
    )(
      implicit httpSupport: HttpSupport[Res],
      mode: Mode[`NetUrl#httpPut`],
      httpTimeout: HttpTimeout,
      httpRedirectConfig: HttpRedirectConfig,
      httpCertificateConfig: HttpCertificateConfig,
      httpBasicAuthentication: HttpBasicAuthentication
    ): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
      mode.wrap(httpSupport.doHttp(res, content, headers, "POST"))
  }
  
  implicit def basicHttpSupport: HttpSupport[HttpUrl] = new HttpSupport[HttpUrl] {
  
    def doHttp[C: PostType, T](res: HttpUrl, content: C, headers: Map[String, String] = Map(), method: String = "POST")
        (implicit mode: Mode[`NetUrl#httpPost`], httpTimeout: HttpTimeout, httpRedirectConfig: HttpRedirectConfig,
        httpCertificateConfig: HttpCertificateConfig, httpBasicAuthentication: HttpBasicAuthentication): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
      mode wrap {
        // FIXME: This will produce a race condition if creating multiple URL connections with
        // different values for followRedirects in parallel
        HttpURLConnection.setFollowRedirects(httpRedirectConfig.follow)
        val conn: URLConnection = new URL(HttpUrl.uriCapable.uri(res).toString).openConnection()
        conn.setConnectTimeout(httpTimeout.duration)
        conn match {
          case c: HttpsURLConnection =>
            if(httpCertificateConfig.ignoreIfInvalid) {
              c.setSSLSocketFactory(NetUrl.sslContext.getSocketFactory)
              c.setHostnameVerifier(NetUrl.allHostsValid)
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
            "Basic "+NetUrl.base64.encode(s"$username:$password".getBytes("UTF-8")).mkString)
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
}

trait HttpSupport[Res] {

  def doHttp[C: PostType, T](
    res: Res,
    content: C,
    headers: Map[String, String] = Map(),
    method: String
  )(
    implicit mode: Mode[`NetUrl#httpPost`],
    httpTimeout: HttpTimeout,
    httpRedirectConfig: HttpRedirectConfig,
    httpCertificateConfig: HttpCertificateConfig,
    httpBasicAuthentication: HttpBasicAuthentication
  ): mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws]
}
