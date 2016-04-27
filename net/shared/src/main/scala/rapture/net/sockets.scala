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

import java.io._

object Tcp {
  /** Listens for incoming connections on the specified port
    *
    * @usecase def listen(port: Int): Input[Byte]
    * @param port the port to listen to */
  def listen[K](port: Int)(implicit ib: InputBuilder[InputStream, K],
      ob: OutputBuilder[OutputStream, K], mode: Mode[`Tcp.listen`]):
      mode.Wrap[(Input[K], Output[K]), Exception] = mode.wrap {
    val sock = new java.net.ServerSocket(port)
    val sock2 = sock.accept()
    (ib.input(sock2.getInputStream), ob.output(sock2.getOutputStream))
  }

  def handle[K](port: Int)(action: (Input[K], Output[K]) => Unit)
      (implicit ib: InputBuilder[InputStream, K], ob: OutputBuilder[OutputStream, K]): Unit = {
    val sock = new java.net.ServerSocket(port)
    while(true) {
      val sock2 = sock.accept()
      Thread.fork(s"rapture-port$port") {
        action(ib.input(sock2.getInputStream), ob.output(sock2.getOutputStream))
      }
    }
  }
  
  /*def listen(port: Int, local: Boolean = true, timeout: Int = 2000) = {
    val socket = new ServerSocket()
    socket.setSoTimeout(timeout)
    if(local) socket.bind(new InetSocketAddress("127.0.0.1", port))
    else socket.bind(new InetSocketAddress(port))
  }*/
}


object SocketUri {
  implicit val socketUri = new UriCapable[SocketUri] {
    def uri(su: SocketUri): Uri = Uri("socket", s"//${su.hostname}:${su.port}")
  }
}

case class SocketUri(val hostname: String, val port: Int) {
  
  lazy val javaSocket: java.net.Socket = new java.net.Socket(hostname, port)
  
  def schemeSpecificPart = "//"+hostname+":"+port
  
  def absolute = true

}

object Socket {
  def apply(hostname: String, port: Int): SocketUri = new SocketUri(hostname, port)
  def apply(hostname: String, svc: services.tcp.Port): SocketUri = new SocketUri(hostname, svc.portNo)

  private val UriMatcher = """socket://([a-z0-9\.]+):([1-9][0-9]*)""".r

  def parse(uri: String) = uri match {
    case UriMatcher(host, port) => new SocketUri(host, port.toInt)
  }
}


