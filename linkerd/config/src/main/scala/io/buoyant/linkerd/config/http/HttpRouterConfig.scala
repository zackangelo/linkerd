package io.buoyant.linkerd.config.http

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.jsontype.NamedType
import com.twitter.finagle.Stack
import com.twitter.finagle.Stack.Params
import io.buoyant.linkerd.config._

case class HttpRouterConfig(
  httpUriInDst: Option[Boolean],
  servers: Option[Seq[BasicServerConfig]]
) extends RouterConfig {
  import HttpRouterConfig._

  def protocolName = Protocol.name

  override private[config] def routerParams(params: Params, servers: Seq[ServerParams]): RouterParams =
    HttpRouterParams(params, servers)
}

object HttpRouterConfig {
  object Protocol {
    val name = "http"
  }

  class Registrar extends NamedRouterConfig[HttpRouterConfig](Protocol.name)
}

case class HttpRouterParams(params: Stack.Params, servers: Seq[ServerParams]) extends RouterParams

