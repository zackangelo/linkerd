package io.buoyant.linkerd

import com.fasterxml.jackson.core.{io => _, _}
import com.twitter.finagle._
import com.twitter.util.Closable

/**
 * A router configuration builder api.
 *
 * Each router must have a [[ProtocolInitializer protocol]] that
 * assists in the parsing and intialization of a router and its
 * services.
 *
 * `params` contains all params configured on this router, including
 * (in order of ascending preference):
 *  - protocol-specific default router parameters
 *  - linker default parameters
 *  - router-specific params.
 *
 * Each router must have one or more [[Server Servers]].
 *
 * Concrete implementations are provided by a [[ProtocolInitializer]].
 */
trait Router {
  def protocol: ProtocolInitializer

  // configuration
  def params: Stack.Params
  protected def _withParams(ps: Stack.Params): Router
  def withParams(ps: Stack.Params): Router = {
    // Copy stats and tracing params from router to servers
    withServers(
      servers.map { s =>
        s.configured(ps[param.Stats])
          .configured(ps[param.Tracer])
      }
    )._withParams(ps)
  }
  def configured[P: Stack.Param](p: P): Router = withParams(params + p)
  def configured(ps: Stack.Params): Router = withParams(params ++ ps)

  // helper aliases
  def label: String = params[param.Label].label

  // servers
  def servers: Seq[Server]
  protected def withServers(servers: Seq[Server]): Router

  /** Return a router with an additional server. */
  def serving(s: Server): Router = withServers(servers :+ Router.configureServer(this, s))

  def serving(ss: Seq[Server]): Router = ss.foldLeft(this)(_ serving _)

  /** Return a router with TLS configuration read from the provided parser. */
  def withTls(tls: TlsClientConfig): Router

  /**
   * Initialize a router by instantiating a downstream router client
   * so that its upstream `servers` may be bound.
   */
  def initialize(): Router.Initialized
}

object Router {
  /**
   * A [[Router]] that has been configured and initialized.
   *
   * Concrete implementations
   */
  trait Initialized extends Closable {
    def protocol: ProtocolInitializer
    def params: Stack.Params
    def servers: Seq[Server.Initializer]
  }

  private def configureServer(router: Router, server: Server): Server = {
    val ip = server.ip.getHostAddress
    val port = server.port
    val param.Stats(stats) = router.params[param.Stats]
    val routerLabel = router.label
    server.configured(param.Label(s"$ip/$port"))
      .configured(Server.RouterLabel(routerLabel))
      .configured(param.Stats(stats.scope(routerLabel, "srv")))
      .configured(router.params[param.Tracer])
  }
}
