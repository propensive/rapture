/******************************************************************************************************************\
* Rapture HTTP, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
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

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListMap

import rapture.mime._
import rapture.codec._
import rapture.uri._

object RequestExtractors {

  /** A standard implementaiton of a response which confirms cross-domain access corntrol */
  def accessControlAllowOrigin(domain: String)(implicit enc: Encoding): Response =
    StreamResponse(200, ("Access-Control-Allow-Origin" -> domain) :: ("Access-Control-Allow-Credentials" -> "true") :: Response.NoCache, MimeTypes.`application/xml`, v => ())(enc)

  /** Method for creating new HTTP header extractors for requests */
  def withHttpHeader(h: String) = new HttpHeader(h)

  class HasParam(p: Symbol) {
    def unapply(r: HttpRequest): Option[Boolean] = Some(r.exists(p))
  }

  class GetParam(p: Symbol) { def unapply(r: HttpRequest): Option[String] = r.param(p) }

  class GetCookie(p: Symbol) {
    def unapply(r: HttpRequest): Option[String] = r.cookie(p)
  }
 
  object & {
    def unapply(r: HttpRequest): Option[(HttpRequest, HttpRequest)] = Some((r, r))
  }

  class HasCookie(p: Symbol) {
    def unapply(r: HttpRequest): Option[Boolean] =
      Some(r.cookie(p).isDefined)
  }

  /** Method for producing new cookie extractors for requests */
  def getCookie(c: Symbol) = new GetCookie(c)
  def hasCookie(c: Symbol) = new HasCookie(c)

  object AsInt { def unapply(s: String): Option[Int] = try Some(s.toInt) catch { case e: Exception => None } }

  /** Extract the path from the request */
  object Path { def unapply(r: HttpRequest): Option[SimplePath] = Some(r.path) }

  /** Defines a pattern matching construct to be used to chain together constraints on requests */
  object ~ { def unapply(r: HttpRequest) = Some((r, r)) }

  /** Method for creating new parameter extractors for requests */
  def getParam(p: Symbol) = new GetParam(p)
  def hasParam(p: Symbol) = new HasParam(p)
  
  class HttpHeader(p: String) { def unapply(r: HttpRequest): Option[String] = r.headers.get(p).flatMap(_.headOption) }

}

