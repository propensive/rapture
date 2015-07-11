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
import rapture.core._
import rapture.io._
import rapture.uri._
import scala.collection.mutable.{HashMap, Queue}

import org.apache.commons.net.ftp._

abstract class ConnectionPool[Resource, Key] {

  private val resources: HashMap[Key, Queue[Resource]] =
    new HashMap[Key, Queue[Resource]]()

  protected def make(key: Key): Resource
  protected def dispose(resource: Resource): Unit
  protected def check(resource: Resource): Boolean
  protected def spare = 1
  protected def timeout = 10*60000L
  
  def acquire(key: Key): Resource = synchronized {
    if(resources.getOrElseUpdate(key, new Queue[Resource]).length == 0) make(key)
    else {
      val r = resources(key).dequeue
      if(check(r)) r else {
        dispose(r)
        make(key)
      }
    }
  }

  val lastLow: HashMap[Key, Long] = new HashMap[Key, Long]()

  def release(key: Key, resource: Resource): Unit = synchronized {
    val now = System.currentTimeMillis()
    if(resources(key).size < spare) lastLow(key) = now
    if(lastLow(key) > now - timeout) resources(key).enqueue(resource)
    else dispose(resource)
  }

  def using[Result](key: Key)(fn: Resource => Result): Result = {
    val res = acquire(key)
    try fn(res) finally release(key, res)
  }
}

case class ConnectionDetails(username: String, password: String, passive: Boolean, hostname: String)

object FtpConnectionPool extends ConnectionPool[FTPClient, ConnectionDetails] {
  
  def dispose(client: FTPClient) = ()

  def check(client: FTPClient) = true
  
  def make(k: ConnectionDetails): FTPClient = {
    val ftp = new FTPClient()
    val config = new FTPClientConfig()
    ftp.configure(config)
    ftp.login(k.username, k.password)
    if(k.passive) ftp.enterLocalPassiveMode()
    ftp.setFileType(FTP.BINARY_FILE_TYPE)
    ftp
  }
}

case class FtpCredentials(username: String, password: String = null)

object Ftp extends Scheme[FtpUrl] {
  def schemeName = "ftp"
  def /(hostname: String, port: Int = 25) = new FtpPathRoot(hostname, port)

  private val FtpRegex =
    """ftp:\/\/([\.\-a-z0-9]+)(:[1-9][0-9]*)?\/(.*)""".r

  def parse(s: String)(implicit mode: Mode[`Ftp.parse`]): mode.Wrap[FtpUrl, Exception] =
    mode.wrap { s match {
      case FtpRegex(server, port, path) =>
        new FtpPathRoot(server, Option(port).map(_.substring(1).toInt).getOrElse(25)).makePath(0, path.split("/"), Map())
    } }

  def connect[C, T]()(fn: C => T)(implicit ftp: FtpSystem[C]) = {
    val conn = ftp.newConnection()
    fn(conn)
  }
}

trait FtpSystem[Connection] {
  def newConnection(): Connection
  def input(conn: Connection, path: String): java.io.InputStream
}

class FtpPathRoot(val hostname: String, val port: Int) extends NetPathRoot[FtpUrl] { thisPathRoot =>
  def scheme = Ftp
 
  def makePath(ascent: Int, elements: Seq[String], afterPath: AfterPath) =
    new FtpUrl(this, elements)

  def /[P <: Path[P]](path: P) = makePath(0, path.elements, Map())

  override def equals(that: Any): Boolean =
    that.isInstanceOf[FtpPathRoot] && hostname == that.asInstanceOf[FtpPathRoot].hostname
}

object FtpUrl {
  implicit def reader[C](implicit cred: FtpCredentials, ftp: FtpSystem[C]): Reader[FtpUrl, Byte] =
    new JavaInputStreamReader[FtpUrl]({ ftpUrl =>
      ftp.input(ftp.newConnection(), ftpUrl.path.mkString("/", "/", ""))
    })
}

class FtpUrl(val pathRoot: NetPathRoot[FtpUrl], val path: Seq[String]) extends
    Url[FtpUrl](path, Map()) with NetUrl {
  def makePath(ascent: Int, xs: Seq[String], afterPath: AfterPath) =
    new FtpUrl(pathRoot, elements)

  def canonicalPort = 25
  def hostname = pathRoot.hostname
  def port: Int = pathRoot.port
  def ssl: Boolean = false
}

/*class FtpSession(username: String, password: String, passive: Boolean) {
  def connect(server: String): Unit = println("Creating connection to server "+server)
  def close(): Unit = println("Closing connection")
}

case class FtpServer(username: String, password: String, passive: Boolean) {
  def connect[T](blk: FtpSession => T) = {
    val c = new FtpSession(username, password, passive)
    c.connect()
    val s = blk(c)
    c.close()
    s
  }
}

implicit def ftpReader(implicit session: FtpSession) = new StreamReader[FtpUrl, Byte] {
  def input(ftp: FtpUrl): ![Exception, Input[Byte]] = except(new Input[Byte] {
    def read() = None
    def ready() = true
    def close() = ()
  })
}*/
