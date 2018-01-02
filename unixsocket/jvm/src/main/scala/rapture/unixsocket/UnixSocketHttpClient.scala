package rapture.unixsocket

import java.io.File
import java.net.{Socket, InetSocketAddress}
import java.util.concurrent.TimeUnit

import org.apache.http.{HttpRequest, HttpHost}
import org.apache.http.config.SocketConfig
import org.apache.http.conn.{ManagedHttpClientConnection, HttpClientConnectionOperator, SchemePortResolver}
import org.apache.http.impl.conn.{PoolingHttpClientConnectionManager, ManagedHttpClientConnectionFactory, DefaultHttpResponseParserFactory, DefaultRoutePlanner}
import org.apache.http.impl.client.HttpClients
import org.apache.http.impl.io.DefaultHttpRequestWriterFactory
import org.apache.http.protocol.HttpContext

import org.newsclub.net.unix.{AFUNIXSocketAddress, AFUNIXSocket}

object UnixSocketHttpClient {

  /**
    * Executes an HttpRequest
    * @param host The custom HttpHost (the hostname is actually the socket file)
    * @param request The HttpRequest (eg. GET /)
    * @return The CloseableHttpResponse
    */
  def execute(host: HttpHost, request: HttpRequest) =
    client.execute(host, request)

  /**
    * Create the customized http client
    */
  private val client = HttpClients
    .custom
    .setConnectionManager(connectionManager)
    .setConnectionManagerShared(true)
    .setRoutePlanner(routePlanner)
    .build

  /**
    * The customized connection manager
    */
  private def connectionManager = new PoolingHttpClientConnectionManager(connectionOperator, connectionFactory, -1, TimeUnit.MILLISECONDS)

  /**
    * A route planner with our custom scheme port resolver
    */
  private def routePlanner = new DefaultRoutePlanner(schemePortResolver)

  /**
    * Our connection operator
    * It will create unix socket connections for us
    */
  private def connectionOperator = new HttpClientConnectionOperator {

    /**
      * Creates the socket for our request, which is a unix socket.
      */
    def connect(conn: ManagedHttpClientConnection, host: HttpHost, localAddress: InetSocketAddress,
                connectTimeout: Int, socketConfig: SocketConfig, context: HttpContext): Unit = {
      val sock: Socket = AFUNIXSocket.newInstance
      sock.setTcpNoDelay(socketConfig.isTcpNoDelay)
      if (socketConfig.getRcvBufSize > 0) {
        sock.setReceiveBufferSize(socketConfig.getRcvBufSize)
      }
      if (socketConfig.getSndBufSize > 0) {
        sock.setSendBufferSize(socketConfig.getSndBufSize)
      }
      val linger: Int = socketConfig.getSoLinger
      if (linger >= 0) {
        sock.setSoLinger(true, linger)
      }
      sock.connect(new AFUNIXSocketAddress(new File(host.getHostName)), connectTimeout)
      conn.bind(sock)
    }

    /**
      * Can upgrade an http connection. This is only used for proxies - so we don't need it
      */
    def upgrade(conn: ManagedHttpClientConnection, host: HttpHost, context: HttpContext) =
      throw new NotImplementedError("upgrade is not supported - a unix-socket cannot be accessed through a proxy")

  }

  /**
    * A default managed connection factory
    */
  private def connectionFactory = new ManagedHttpClientConnectionFactory(
    new DefaultHttpRequestWriterFactory(),
    new DefaultHttpResponseParserFactory()
  )

  /**
    * Resolves the port for the "unix" scheme
    * We don't actually need a port, but an NPE will be thrown if we don't replace it.
    */
  private def schemePortResolver = new SchemePortResolver { def resolve(host: HttpHost) = 0 }

}

