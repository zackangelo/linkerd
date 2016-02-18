package io.buoyant.linkerd

import com.fasterxml.jackson.annotation.{JsonIgnore, JsonTypeInfo}
import com.fasterxml.jackson.databind.ObjectMapper
import com.twitter.conversions.time._
import com.twitter.finagle.service.{TimeoutFilter, FailFastFactory}
import com.twitter.finagle._
import com.twitter.finagle.param.Label
import com.twitter.finagle.server.StackServer
import com.twitter.util.Time
import io.buoyant.linkerd.ProtocolInitializer.ParamsMaybeWith
import io.buoyant.router._
import java.net.InetSocketAddress

/**
 * Provides a protocol-agnostic interface for protocol-specific
 * configuration & initialization.  Adapts linkerd's [[Router]] with
 * `io.buoyant.router.Router` and [[Server]] to
 * `com.twitter.finagle.Server`.
 *
 * Furthermore a protocol may provide parsers for protocol-specific
 * configuration parameters.
 *
 */
trait ProtocolInitializer {
  import ProtocolInitializer._

  /** The protocol name, as read from configuration. */
  def name: String

  def registerSubtypes(mapper: ObjectMapper): Unit

  /*
   * Router configuration & initialization
   */
  protected type RouterReq
  protected type RouterRsp

  /** The default protocol-specific router configuration */
  protected def defaultRouter: StackRouter[RouterReq, RouterRsp]

  /**
   * Satisfies the protocol-agnostic linkerd Router interface by
   * wrapping the protocol-specific router stack.
   */
  private case class ProtocolRouter(
    router: StackRouter[RouterReq, RouterRsp],
    servers: Seq[Server] = Nil
  ) extends Router {
    def params = router.params
    def withParams(ps: Stack.Params): Router = copy(router = router.withParams(ps))

    protected def withServers(ss: Seq[Server]): Router = copy(servers = ss)

    val protocol = ProtocolInitializer.this
    def initialize(): Router.Initialized = {
      if (servers.isEmpty) {
        val Label(name) = params[Label]
        throw new IllegalStateException(s"router '$name' has no servers")
      }
      val factory = router.factory()
      val adapted = adapter.andThen(factory)
      val servable = servers.map { server =>
        val stackServer = defaultServer.withParams(server.params)
        ServerInitializer(protocol, server.addr, stackServer, adapted)
      }
      InitializedRouter(protocol, params, factory, servable)
    }

    // TODO: Fix client params
    override def withTls(tls: TlsClientConfig): Router = {
      val tlsPrep = tls.tlsClientPrep[RouterReq, RouterRsp]
      val clientStack = router.clientStack.replace(Stack.Role("TlsClientPrep"), tlsPrep)
      copy(router = router.withClientStack(clientStack))
    }
  }

  def router: Router = ProtocolRouter(defaultRouter)
    .configured(Label(name))

  /*
   * Server initialization
   */
  protected type ServerReq
  protected type ServerRsp

  /** Adapts a server to a router */
  protected def adapter: Filter[ServerReq, ServerRsp, RouterReq, RouterRsp]

  /** The default protocol-specific server configuration */
  protected def defaultServer: StackServer[ServerReq, ServerRsp]

  def defaultServerPort: Int
}

object ProtocolInitializer {

  /**
   * A [[ProtocolInitializer]] whose Server and Router have identical
   * request and response types.
   */
  trait Simple extends ProtocolInitializer {
    protected type Req
    protected type Rsp
    protected final type RouterReq = Req
    protected final type RouterRsp = Rsp
    protected final type ServerReq = Req
    protected final type ServerRsp = Rsp
    protected val adapter = Filter.identity[Req, Rsp]
  }

  /** Protocol-aware implementation of [[Router.Initialized]]. */
  private case class InitializedRouter[Req, Rsp](
    protocol: ProtocolInitializer,
    params: Stack.Params,
    factory: ServiceFactory[Req, Rsp],
    servers: Seq[Server.Initializer]
  ) extends Router.Initialized {
    def name: String = params[Label].label
    def close(t: Time) = factory.close(t)
  }

  /** Protocol-aware implementation of [[Server.Initializer]]. */
  private case class ServerInitializer[Req, Rsp](
    protocol: ProtocolInitializer,
    addr: InetSocketAddress,
    server: StackServer[Req, Rsp],
    factory: ServiceFactory[Req, Rsp]
  ) extends Server.Initializer {
    def params = server.params
    def router: String = server.params[Server.RouterLabel].label
    def ip = addr.getAddress
    def port = addr.getPort
    def serve() = server.serve(addr, factory)
  }

  implicit class ParamsMaybeWith(val params: Stack.Params) extends AnyVal {
    def maybeWith[T: Stack.Param](p: Option[T]): Stack.Params = {
      p match {
        case Some(t) => params + t
        case None => params
      }
    }
  }
}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "protocol")
abstract class RouterConfig {
  var servers: Seq[ServerConfig] = Nil
  var client: Option[ClientConfig] = None
  var baseDtab: Option[Dtab] = None
  var failFast: Option[Boolean] = None
  var timeoutMs: Option[Int] = None
  var label: Option[String] = Some(protocol.name)
  var dstPrefix: Option[String] = None
  var tls: Option[TlsClientConfig] = None

  @JsonIgnore
  def routerParams = Stack.Params.empty
    .maybeWith(baseDtab.map(dtab => RoutingFactory.BaseDtab(() => dtab)))
    .maybeWith(label.map(Label(_)))
    .maybeWith(failFast.map(FailFastFactory.FailFast(_)))
    .maybeWith(timeoutMs.map(timeout => TimeoutFilter.Param(timeout.millis)))
    .maybeWith(dstPrefix.map(pfx => RoutingFactory.DstPrefix(Path.read(pfx))))

  @JsonIgnore
  def router(params: Stack.Params): Router = {
    protocol.router.withParams(params ++ routerParams).serving(
      servers.map(_.mk(protocol, routerParams[Label].label))
    )
  }

  @JsonIgnore
  def protocol: ProtocolInitializer
}

  class ClientConfig {
    def clientParams: Stack.Params = Stack.Params.empty
  }

