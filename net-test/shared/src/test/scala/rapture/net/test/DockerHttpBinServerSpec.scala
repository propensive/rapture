package rapture.net.test

import org.scalatest.concurrent.Eventually._
import org.scalatest.concurrent.PatienceConfiguration._
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfter, Suite}
import rapture.net._
import rapture.net.test.helper.{DockerContainer, ExposePort}

trait DockerHttpBinServerSpec extends BeforeAndAfter {
  this: Suite =>

  val SERVER_PORT = 33881
  val SERVER_URL = s"http://127.0.0.1:$SERVER_PORT"
  private var startedDockerContainer: DockerContainer#StartedDockerContainer = _

  before {
    startedDockerContainer =
      new DockerContainer("paddycarey/httpbin", Set(ExposePort(SERVER_PORT.toString, "8000"))).startContainer()
    eventually(Timeout(Span(30, Seconds))) {
      println("Trying to connect....")
      assert(Http("127.0.0.1", SERVER_PORT).httpGet().status == 200)
    }
  }

  after {
    startedDockerContainer.killAndRemoveContainer()
  }

}
