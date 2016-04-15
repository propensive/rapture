/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.http

import rapture.json._
import rapture.data._
import rapture.mime._
import rapture.io._
import rapture.net._
import rapture.codec._, encodings.`UTF-8`._

package object jsonInterop {
  implicit def jsonHttpHandler(implicit formatter: Formatter[JsonAst] { type Out = String }): HttpHandler[Json] =
    new HttpHandler[Json] {
      def response(j: Json): Response = StreamResponse(200, Response.NoCache, MimeTypes.`application/json`,
          { os =>
	Json.format(j).input[Char] > os
	os.close()
      })
  }

  implicit def jsonPostType(implicit formatter: Formatter[JsonAst] { type Out = String }): PostType[Json] = new PostType[Json] {
    def contentType = Some(MimeTypes.`application/json`)
    def sender(content: Json): Input[Byte] =
      Json.format(content).input[Byte]
  }
}


