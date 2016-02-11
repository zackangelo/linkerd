package io.buoyant.linkerd.config.thrift

import com.twitter.finagle.Stack.Params
import io.buoyant.linkerd.config._

case class ThriftRouterConfig(
  thriftFramed: Option[Boolean],
  thriftMethodInDst: Option[Boolean],
  // TODO: implement ThriftServerConfig
  servers: Option[Seq[BasicServerConfig]]
) extends RouterConfig  {
  import ThriftRouterConfig._
  def protocolName = Protocol.name

  override private[config] def routerParams(params: Params, servers: Seq[ServerParams]): RouterParams =
    ThriftRouterParams(params, servers)
}

object ThriftRouterConfig {
  object Protocol {
    val name = "thrift"
  }

  class Registrar extends NamedRouterConfig[ThriftRouterConfig](Protocol.name)
}

case class ThriftRouterParams(params: Params, servers: Seq[ServerParams]) extends RouterParams
