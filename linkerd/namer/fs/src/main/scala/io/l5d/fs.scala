package io.l5d

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import com.fasterxml.jackson.databind.DeserializationContext
import com.twitter.finagle.{Path, Stack}
import com.twitter.util.{Throw, Try}
import io.buoyant.linkerd.config.{RootDirNotDirectory, ConfigDeserializer}
import io.buoyant.linkerd.config.types.Directory
import io.buoyant.linkerd.{NamerConfig, NamerInitializer, Parsing}
import io.buoyant.linkerd.namer.fs.WatchingNamer
import io.l5d.fs.RootDir
import java.nio.file.{Path => NioPath, Paths}

object fs {
  case class RootDir(path: Option[NioPath]) {
    require(path.forall(_.toFile.isDirectory), s"$path is not a directory")
  }

  implicit object RootDir extends Stack.Param[RootDir] {
    val default = RootDir(None)
  }

  val defaultParams = Stack.Params.empty +
    NamerInitializer.Prefix(Path.Utf8("io.l5d.fs"))

  case class Initializer(params: Stack.Params) extends NamerInitializer(params) {
    def this() = this(defaultParams)
    def withParams(ps: Stack.Params) = copy(ps)

    def newNamer() = params[fs.RootDir] match {
      case fs.RootDir(None) => throw new IllegalArgumentException("io.l5d.fs requires a 'rootDir'")
      case fs.RootDir(Some(path)) => new WatchingNamer(path, params[NamerInitializer.Prefix].path)
    }
  }
}

case class fs(rootDir: Option[NioPath]) extends NamerConfig {
  override def defaultParams = fs.defaultParams
  override def params: Try[Stack.Params] =
    rootDir match {
      case r@Some(dir) if dir.toFile.isDirectory => super.params.map(_ + RootDir(r))
      case Some(path) => Throw(RootDirNotDirectory(path))
      case None => super.params
    }

  def initializerFactory = fs.Initializer.apply
}


