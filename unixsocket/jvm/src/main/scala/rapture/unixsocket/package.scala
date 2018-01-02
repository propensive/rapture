package rapture

import rapture.core.`package`._
import rapture.core.Mode
import rapture.io.OutputStreamBuilder
import rapture.io.`package`._
import rapture.net._
import rapture.uri.UriContext

import java.io._

import org.apache.http.HttpHost
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods._
import org.apache.http.entity.AbstractHttpEntity

package object unixsocket {


  implicit class EnrichedUnixUriContext(uc: UriContext.type) {
    // `unix+http` as method name did not play well with the implicit conversion it seems :(
    def unix(constants: List[String])(variables: List[String]) = {
      var unixSocketUrl = "unix:" + constants.zip(variables :+ "").map { case (a, b) => a+b }.mkString
      UnixSocketHttpUrl.parse(unixSocketUrl)
    }
  }


  implicit def basicHttpSupport: HttpSupport[UnixSocketHttpUrl] = new HttpSupport[UnixSocketHttpUrl] {

    def doHttp[C: PostType, T](unixSocketHttpUrl: UnixSocketHttpUrl, content: C,
                               headers: Map[String, String] = Map(),method: String = "POST")
                              (implicit mode: Mode[`NetUrl#httpPost`], httpTimeout: HttpTimeout,
                               httpRedirectConfig: HttpRedirectConfig, httpCertificateConfig: HttpCertificateConfig,
                               httpBasicAuthentication: HttpBasicAuthentication):
                              mode.Wrap[HttpResponse, HttpExceptions with httpTimeout.Throws] =
      mode wrap {

        val host = HttpHost.create(s"unix://${unixSocketHttpUrl.socketFileName}")

        val url = unixSocketHttpUrl.queryString + unixSocketHttpUrl.queryParams.getOrElse("")

        class InvertedHttpEntity extends AbstractHttpEntity {

          override def isRepeatable: Boolean = false

          override def getContentLength: Long = -1

          override def isStreaming: Boolean = false

          override def getContent: InputStream = throw new UnsupportedOperationException

          override def writeTo(outputStream: OutputStream): Unit = {
            ensuring(OutputStreamBuilder.output(outputStream)) { out =>
              ?[PostType[C]].sender(content) > out
            }
          }

        }

        val request = method.toUpperCase match {
          case "GET"    => new HttpGet(url)
          case "POST"   => {
            val post = new HttpPost(url)
            if(content != None)
              post.setEntity(new InvertedHttpEntity)
            post
          }
          case "PUT"    => {
            val put = new HttpPut(url)
            if(content != None)
              put.setEntity(new InvertedHttpEntity)
            put
          }
          case "DELETE" => new HttpDelete(url)
        }

        val config = RequestConfig
          .custom()
          .setConnectTimeout(httpTimeout.duration)
          .build

        request.setConfig(config)

        def base64encode(content: String) =
          NetUrl.base64.encode(content.getBytes("UTF-8")).mkString

        httpBasicAuthentication.credentials foreach {
          case (username, password) =>
            request.setHeader("Authorization", "Basic " + base64encode(s"$username:$password"))
        }

        ?[PostType[C]]
          .contentType
          .foreach { ct => request.setHeader("Content-Type", ct.name) }

        for((k, v) <- headers) request.setHeader(k, v)

        val response = UnixSocketHttpClient.execute(host, request)

        val statusLine = response.getStatusLine

        val statusCode = statusLine.getStatusCode

        val responseHeaders = response.getAllHeaders.map(header => (header.getName, List(header.getValue))).toMap

        new HttpResponse(responseHeaders, statusCode, response.getEntity.getContent)
      }
  }

}
