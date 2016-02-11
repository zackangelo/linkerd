package io.l5d.experimental

import com.twitter.finagle.param.Label
import com.twitter.finagle.{Http, Path, Stack}
import com.twitter.util.Try
import io.buoyant.consul.{CatalogNamer, v1, SetHostFilter}
import io.buoyant.linkerd.{NamerConfig, NamerInitializer, Parsing}
import io.buoyant.linkerd.config.types.Port
import io.l5d.experimental.consul.Host

/**
 * Supports namer configurations in the form:
 *
 * <pre>
 * namers:
 * - kind: io.l5d.experimental.consul
 *   host: consul.site.biz
 *   port: 8600
 * </pre>
 */
object consul {
  /** The consul host; default: localhost */
  case class Host(host: String)
  implicit object Host extends Stack.Param[Host] {
    val default = Host("localhost")
  }

  /** The consul port; default: 8500 */
  implicit object Port extends Stack.Param[Port] {
    val default = Port(8500)
  }

  val defaultParams = Stack.Params.empty +
    NamerInitializer.Prefix(Path.Utf8("io.l5d.consul"))

  /**
    * Configures a Consul namer.
    */
  case class Initializer(params: Stack.Params) extends NamerInitializer(params) {
    def this() = this(consul.defaultParams)
    def withParams(ps: Stack.Params) = copy(ps)
    /**
      * Build a Namer backed by Consul.
      */
    def newNamer(): CatalogNamer = {
      val consul.Host(host) = params[consul.Host]
      val Port(port) = params[Port]
      val path = params[NamerInitializer.Prefix].path.show
      val service = Http.client
        .configured(Label("namer" + path))
        .filtered(new SetHostFilter(host, port))
        .newService(s"/$$/inet/$host/$port")

      def mkNs(ns: String) = v1.Api(service)
      new CatalogNamer(prefix, mkNs)
    }
  }
}

case class consul(
  host: Option[String],
  port: Option[Port]) extends NamerConfig {

  override def defaultParams = super.defaultParams ++ consul.defaultParams

  override def params: Try[Stack.Params] = super.params map { ps =>
    Seq(host.map(Host(_)), port).flatten.foldLeft(ps)(_ + _)
  }

  override def initializerFactory = consul.Initializer.apply
}



