package io.buoyant.linkerd.config

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.twitter.finagle.{Dtab, Stack}
import com.twitter.util.{Throw, Return, Try}
import scala.util.Success

/**
  * LinkerConfig can configure some "default" settings for its routers; this trait
  * allows those parameters to be shared.
  */
trait CommonRouterConfig {
  var baseDtab: Option[Dtab] = None
  var failFast: Option[Boolean] = None
  var timeoutMs: Option[Int] = None
}


/*
 * RouterConfig implements the generic configuration for a [[Router]]. All
 * Routers must have a protocol (i.e. Thrift, HTTP); those are represented
 * as subclasses of RouterConfig.
 *
 * To implement a protocol:
 * * Create a case class subclassing the RouterConfig trait that adds any
 *   extra properties which should be serialized/deserialized to configuration.
 *   This must implement a `protocol` method, which is described below.
 * * Implement a ConfigRegistrar class, with a Register method to add
 *   the case class above to Jackson's ObjectMapper.
 * * Implement any necessary validation logic in a validated() method, which
 *   should return a RouterParams instance.
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "protocol")
trait RouterConfig extends CommonRouterConfig {
  // These are vars to allow Jackson to deserialize instances without subclasses needing to know about
  // the base trait's properties.
  var label: Option[String] = None
  var dstPrefix: Option[String] = None

  def servers: Option[Seq[ServerConfig]]

  // The following section must be implemented by protocols.
  def protocolName: String
  def defaultServer: ServerConfig = BasicServerConfig(None, None)
  // A protocol implementation must provide both a Server and Router type.
  private[config] def validatedServers: Try[Seq[ServerParams]] = {
    Return(Nil)
  }
  private[config] def validatedParams: Try[Stack.Params] = {

    Return(Stack.Params.empty)
  }
  private[config] def routerParams(params: Stack.Params, servers: Seq[ServerParams]): RouterParams

  def validated(linker: LinkerConfig, others: Seq[RouterConfig]): Try[RouterParams] = {
    for {
      params <- validatedParams
      servers <- validatedServers
    } yield routerParams(params, servers)
  }

}


object RouterConfig {

  /*
  class Defaults(base: RouterConfig, protocol: RouterProtocol, linker: LinkerConfig) {
    def label: String = base.label getOrElse protocol.name
    def failFast: Boolean = base.failFast orElse linker.failFast getOrElse false
    def baseDtab: Option[Dtab] = base.baseDtab orElse linker.baseDtab
    def servers: Seq[ServerConfig] = base.servers getOrElse Seq(base.defaultServer)

    def validated(others: Seq[RouterConfig.Defaults]): ValidatedConfig[RouterParams] = {
      def validatedBaseDtab: ValidatedConfig[Dtab] = {
        try {
          valid(Dtab.read(baseDtab))
        } catch {
          case ex: IllegalArgumentException => invalid(InvalidDtab(baseDtab, ex))
        }
      }

      def validatedLabel: ValidatedConfig[String] =
        if (others.exists(_.label == label))
          invalid(ConflictingLabels(label))
        else
          valid(label)

      // TODO: determine if we need to optimize this by passing it in the foldLeft
      val prevServers = for {
        router <- others
        server <- router.servers
      } yield server.withDefaults(this)

      val validatedServers = ServerConfig.validateServers(servers, this, prevServers)

      Apply[ValidatedConfig].map3(validatedLabel, protocol.validated, validatedServers) {  (label, protocol, servers) =>
        new Validated(label, failFast, baseDtab, protocol, servers)
      }
    }
  }

  class Validated(
    val label: String,
    val failFast: Boolean,
    val baseDtab: Option[Dtab],
    val protocol: RouterProtocol,
    val servers: Seq[ServerConfig.Validated]
  )
  */
  def validateRouters(linker: LinkerConfig, routers: Seq[RouterConfig]): Try[Seq[RouterParams]] = {

    val (validatedRouters, _) = routers.foldLeft((Seq.empty[Try[RouterParams]], Seq.empty[RouterConfig])) {
      case ((accum, prev), r) =>
        (accum :+ r.validated(linker, prev), prev :+ r)
    }
    Try.collect(validatedRouters)
  }

}

trait RouterParams extends ConfigParams {
  def servers: Seq[ServerParams]
}
