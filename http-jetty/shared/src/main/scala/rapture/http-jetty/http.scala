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

package httpBackends {
  object jetty {
    implicit val implicitHttpServer = new HttpBackend {
      import org.eclipse.jetty.server.Server
      import org.eclipse.jetty.servlet.ServletContextHandler
      import org.eclipse.jetty.servlet.ServletHolder

      private var server: Option[Server] = None
      
      class HttpServletWrapper(fn: HttpRequest => Response) extends ServletWrapper {
        def handle(r: HttpRequest) = fn(r)
      }

      def startListening(port: Int)(handler: HttpRequest => Response): Unit = {
        import org.eclipse.jetty.util.log.Logger
        object NoLogging extends Logger {
          override def getName = "no"
          override def warn(msg: String, args: AnyRef*) = ()
          override def warn(thrown: Throwable) = ()
          override def warn(msg: String, thrown: Throwable) = ()
          override def info(msg: String, args: AnyRef*) = ()
          override def info(thrown: Throwable) = ()
          override def info(msg: String, thrown: Throwable) = ()
          override def isDebugEnabled(): Boolean = false
          override def setDebugEnabled(enabled: Boolean) = ()
          override def debug(msg: String, args: AnyRef*) = ()
          override def debug(thrown: Throwable) = ()
          override def debug(msg: String, thrown: Throwable) = ()
          override def getLogger(name: String): Logger = this
          override def ignore(thrown: Throwable) = ()
        }
        org.eclipse.jetty.util.log.Log.setLog(NoLogging)
        server = Some(new Server(port))
        val sch = new ServletContextHandler(server.get, "/", true, false)
        val httpServlet = new HttpServletWrapper(handler)
        sch.addServlet(new ServletHolder(httpServlet), "/")
        server.foreach(_.start())
      }
      
      def stopListening(): Unit = server.foreach(_.stop())
    }
  }
}
