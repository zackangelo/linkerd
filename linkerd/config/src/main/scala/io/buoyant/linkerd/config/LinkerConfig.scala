package io.buoyant.linkerd.config

import com.twitter.finagle.{Namer, Stack, Dtab}
import com.twitter.util.{Throw, Try}

case class LinkerConfig(namers: Option[Seq[NamerConfig]], routers: Option[Seq[RouterConfig]]) extends CommonRouterConfig {
  // Currently, the only thing we require of a Linker is that it has at least one Router configured.
  def validated: Try[LinkerParams] = {
    def validatedRouters: Try[Seq[RouterParams]] = routers
        .filterNot(_.isEmpty)
        .map(RouterConfig.validateRouters(this, _))
        .getOrElse(Throw(NoRoutersSpecified))

    def validatedNamers: Try[Seq[Namer]] =
      Try.collect(namers.getOrElse(Nil).map(_.namer))

    for {
      routers <- validatedRouters
      namers <- validatedNamers
    } yield LinkerParams(Stack.Params.empty, namers, routers)
  }
}


private[config] case class LinkerParams(params: Stack.Params, namers: Seq[Namer], routers: Seq[RouterParams])
  extends ConfigParams


