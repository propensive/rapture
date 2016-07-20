package rapture.net.test.helper

import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.{ContainerConfig, ContainerCreation, HostConfig, PortBinding}
import rapture.core.Mode

import scala.collection.JavaConverters._

case class ExposePort(externalPort: String, internalPort: String)

class DockerContainer(image: String, exposedPorts: Set[ExposePort]) {
  import java.util

  lazy val docker = DefaultDockerClient.fromEnv().build()

  def startContainer()(implicit mode: Mode[_]): mode.Wrap[StartedDockerContainer, Nothing] = mode.wrap {
    docker.pull(image)
    val portBindings = new util.HashMap[String, util.List[PortBinding]]
    exposedPorts.foreach { port =>
      portBindings.put(port.internalPort, List(PortBinding.of("0.0.0.0", port.externalPort)).asJava)
    }
    val containerConfig = ContainerConfig
      .builder()
      .hostConfig(HostConfig.builder().portBindings(portBindings).build())
      .image(image)
      .exposedPorts(exposedPorts.map(_.internalPort).asJava)
      .build()
    val creation = docker.createContainer(containerConfig)
    docker.startContainer(creation.id())
    new StartedDockerContainer(image, exposedPorts, creation)
  }

  class StartedDockerContainer(image: String, exposedPorts: Set[ExposePort], creation: ContainerCreation) {

    def killAndRemoveContainer(): Unit = {
      docker.killContainer(creation.id)
      docker.removeContainer(creation.id)
    }
  }

}
