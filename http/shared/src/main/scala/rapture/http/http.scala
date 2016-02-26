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

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListMap

import rapture.core._

trait `HttpServer.listen` extends MethodConstraint
trait `HttpServer#stop` extends MethodConstraint

trait HttpBackend {
  def startListening(port: Int)(handler: HttpRequest => Response): Unit
  def stopListening(): Unit
}

case class ListenException(msg: String) extends RuntimeException

case class Listening(port: Int)

case class PortListenException(port: Int)

object HttpServer {
  def listen(port: Int = 80)(handler: HttpRequest => Response)(implicit backend: HttpBackend,
      mode: Mode[`HttpServer.listen`]): HttpServer = {
    val httpServer = new HttpServer(port) {
      protected def handle(r: HttpRequest) = handler(r)
    }
    httpServer.start()
    httpServer
  }
}

/** This trait provides a nice interface to the HTTP server */
abstract class HttpServer(val serverPort: Int = 80)(implicit httpBackend: HttpBackend) {

  private[HttpServer] def start()(implicit mode: Mode[_]): mode.Wrap[Listening, ListenException] = mode.wrap {
    httpBackend.startListening(serverPort)(handle)
    Listening(serverPort)
  }

  def stop()(implicit mode: Mode[`HttpServer#stop`]): mode.Wrap[Unit, ListenException] =
    mode.wrap(httpBackend.stopListening())

  protected def handle(r: HttpRequest): Response

  def notFound(r: HttpRequest): Response =
    ErrorResponse(404, Nil, "Not found", "The requested resource could not be found")

  def error(r: HttpRequest, e: Throwable): Response =
    ErrorResponse(500, Nil, "An unexpected error has occurred", "Unknown error")
}
