package io.buoyant.linkerd.config.namers

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.jsontype.NamedType
import io.buoyant.linkerd.config._
import io.buoyant.linkerd.config.types.Directory
import java.nio.file.{InvalidPathException, Path => NioPath, Paths}

case class FileSystemNamerConfig(rootDir: Directory) extends NamerConfig {
  def kind = FileSystemNamerConfig.Protocol.kind
}

object FileSystemNamerConfig {
  object Protocol {
    def kind = "io.l5d.fs" // TODO: switch to using the actual class name once we can avoid conflicts with existing system
  }

  class Registrar extends NamedNamerConfig[FileSystemNamerConfig](Protocol.kind)
}


// This is temporary! Eventually we will use classpaths for NamerConfigs.
/*class FileSystemNamerConfigRegistrar extends ConfigRegistrar {
  def register(mapper: ObjectMapper): Unit =
    mapper.registerSubtypes(new NamedType(classOf[FileSystemNamerConfig], FileSystemNamerConfig.Protocol.kind))
}
*/
