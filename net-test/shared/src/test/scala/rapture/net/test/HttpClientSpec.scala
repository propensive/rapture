package rapture.net.test

import org.scalatest.{Matchers, WordSpec}
import rapture.io._
import rapture.json.Json
import rapture.json.jsonBackends.circe._
import rapture.net.{Http, HttpQuery}

class HttpClientSpec extends WordSpec with Matchers with DockerHttpBinServerSpec {

  "Http client" should {
    "make GET request and get 200 OK status" in {
      val result = Http.parse(s"$SERVER_URL/get").httpGet()
      result.status shouldEqual 200
    }

    "make GET request with query params and get 200 OK status" in {
      val result =
        HttpQuery.parse(s"$SERVER_URL/get?foo=333&bar=abc").httpGet(Map("Content-Type" -> "application/json"))

      result.status shouldEqual 200
      val json = Json.parse(result.slurp[Char])
      json.args.foo.as[String] shouldEqual "333"
      json.args.bar.as[String] shouldEqual "abc"
    }

    "make GET request with custom headers and get 200 OK" in {
      val result = HttpQuery
        .parse(s"$SERVER_URL/get")
        .httpGet(Map("Content-Type" -> "application/json", "Mytest-Header" -> "111-222-333-abc"))

      result.status shouldEqual 200
      val json = Json.parse(result.slurp[Char])
      json.headers.`Mytest-Header`.as[String] shouldEqual "111-222-333-abc"
    }
  }

}
