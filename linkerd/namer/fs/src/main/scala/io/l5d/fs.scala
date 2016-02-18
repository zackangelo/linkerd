package io.l5d

import com.fasterxml.jackson.annotation.JsonIgnore
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.jsontype.NamedType
import com.twitter.finagle.Path
import io.buoyant.linkerd.config.Parser
import io.buoyant.linkerd.namer.fs.WatchingNamer
import io.buoyant.linkerd.{NamerConfig, NamerInitializer}
import io.l5d.fs.FsConfig
import java.nio.file.{Path => NioPath}

class fs extends NamerInitializer {
  /** Register config subtype */
  override def register(mapper: ObjectMapper): Unit = {
    mapper.registerSubtypes(new NamedType(Parser.jClass[FsConfig], "io.l5d.fs"))
  }
}

object fs {
  case class FsConfig(path: Option[NioPath]) extends NamerConfig {
    @JsonIgnore
    override def defaultPrefix: Path = Path.read("io.l5d.fs")

    /**
      * Construct a namer.
      */
    @JsonIgnore
    def newNamer() = path match {
      case None => throw new IllegalArgumentException("io.l5d.fs requires a 'rootDir'")
      case Some(path) => new WatchingNamer(path, prefix.getOrElse(defaultPrefix))
    }
  }
}
