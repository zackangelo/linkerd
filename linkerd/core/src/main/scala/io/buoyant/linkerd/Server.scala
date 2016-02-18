package io.buoyant.linkerd

import com.fasterxml.jackson.annotation.JsonIgnore
import com.twitter.finagle.ssl.Ssl
import com.twitter.finagle.transport.Transport
import com.twitter.finagle.{ListeningServer, Stack}
import io.buoyant.linkerd.config.types.Port
import io.buoyant.linkerd.ProtocolInitializer.ParamsMaybeWith
import java.net.{InetAddress, InetSocketAddress}

/**
 * A Server configuration, describing a request-receiving interface
 * for a [[Router]].
 *
 * Concrete implementations are provided by [[ProtocolInitializer]].
 */
trait Server {
  def protocol: ProtocolInitializer

  def params: Stack.Params

  def router: String
  def label: String
  def ip: InetAddress

  def port: Int

  def addr: InetSocketAddress = new InetSocketAddress(ip, port)
}

object Server {

  case class RouterLabel(label: String)

  implicit object RouterLabel extends Stack.Param[RouterLabel] {
    val default = RouterLabel("")
  }

  /**
   * A [[Server]] that is fully configured but not yet listening.
   */
  trait Initializer {
    def protocol: ProtocolInitializer

    def params: Stack.Params

    def router: String

    def ip: InetAddress

    def port: Int

    def addr: InetSocketAddress

    def serve(): ListeningServer
  }
}

case class ServerConfig(
  port: Option[Port],
  ip: Option[InetAddress],
  tls: Option[TlsServerConfig],
  label: Option[String]
) { config =>

  @JsonIgnore
  private[this] def tlsParam(certificatePath: String, keyPath: String) =
    Transport.TLSServerEngine(
      Some(() => Ssl.server(certificatePath, keyPath, null, null, null))
    )

  @JsonIgnore
  def mk(pi: ProtocolInitializer, routerLabel: String) = new Server {
    override def router: String = routerLabel

    override def ip: InetAddress = config.ip.getOrElse(InetAddress.getLoopbackAddress)

    override def label: String = config.label.getOrElse(routerLabel)

    override def port: Int = config.port.map(_.port).getOrElse(0)

    override def protocol: ProtocolInitializer = pi

    override def params: Stack.Params = Stack.Params.empty
      .maybeWith(tls.map {
        case TlsServerConfig(certPath, keyPath) => tlsParam(certPath, keyPath)
      })
  }
}

case class TlsServerConfig(certPath: String, keyPath: String)
