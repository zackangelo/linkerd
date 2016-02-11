package io.l5d

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import com.twitter.finagle.{Path, Stack}
import io.buoyant.linkerd.config.types.Port
import io.buoyant.linkerd.namer.serversets.ServersetNamer
import io.buoyant.linkerd.{NamerConfig, NamerInitializer, Parsing}
import io.l5d.serversets.ZkConnectString

object serversets {

  case class ZkAddr(hostname: Option[String], port: Option[Port] = Some(Port(2181))) {
    def zkAddr: String = {
      (hostname, port) match {
        case (Some(h), Some(Port(p))) => s"$h:$p"
        case (None, _) => throw new IllegalArgumentException("zkAddrs must contain a host parameter")
        case (_, None) => throw new IllegalArgumentException("zkAddrs must contain a port parameter")
      }
    }
  }


  /**
   * A zookeeper 'connect string' that is used to build the underlying client.
   */
  case class ZkConnectString(connectString: String)
  implicit object ZkConnectString extends Stack.Param[ZkConnectString] {
    val default = ZkConnectString("")
  }

  val defaultParams = Stack.Params.empty +
    NamerInitializer.Prefix(Path.Utf8("io.l5d.serversets"))

  case class Initializer(params: Stack.Params) extends NamerInitializer(params) {
    def this() = this(serversets.defaultParams)
    def withParams(ps: Stack.Params) = copy(ps)

    def newNamer() = {
      val zkAddrs = params[ZkConnectString]
      if (zkAddrs.connectString.nonEmpty)
        new ServersetNamer(zkAddrs.connectString)
      else
        throw new IllegalArgumentException("io.l5d.serversets requires a 'zkAddrs'")
    }
  }

}

case class serversets(zkAddrs: Seq[serversets.ZkAddr]) extends NamerConfig {
  override def defaultParams = serversets.defaultParams
  override def params = super.params map { _ + ZkConnectString(zkAddrs.map(_.zkAddr).mkString(",")) }
  def initializerFactory = serversets.Initializer.apply
}
