package io.buoyant.linkerd.config

import com.google.common.net.InetAddresses
import com.twitter.finagle.Stack
import com.twitter.util.{Return, Try}
import io.buoyant.linkerd.config.types._
import java.net.{InetAddress, InetSocketAddress}

trait ServerConfig {
  def ip: Option[InetAddress]
  def port: Option[Port]

  // TODO: unify this code with what's in Server
  private[this] val loopbackIp = InetAddress.getLoopbackAddress
  private[this] val anyIp = InetAddress.getByAddress(Array[Byte](0, 0, 0, 0))
  private[this] val defaultIp = loopbackIp
  private[this] val defaultPort = Port(0)

  def addr: InetSocketAddress = new InetSocketAddress(
    ip getOrElse loopbackIp,
    (port getOrElse defaultPort).port
  )

  def validated(router: RouterConfig, prevServers: Seq[ServerConfig]): Try[ServerParams]
}

object ServerConfig {
  /*
  class Defaults(base: ServerConfig, router: RouterConfig.Defaults) {
    def ip = base.ip
    def port = base.port
    def addr = base.addr



    def validPort(port: Int): Boolean = MinValue <= port && port <= MaxValue

    def validated(others: Seq[Defaults]): ValidatedConfig[Validated] = {
      // TODO: unify this with code in Server.scala
      def conflicts(other: Defaults): Option[ConflictingPorts] = {
        val addr0 = other.addr
        val addr1 = this.addr
        val conflict = (addr1.getPort != 0) && (addr0.getPort == addr1.getPort) && {
          val (a0, a1) = (addr0.getAddress, addr1.getAddress)
          a0.isAnyLocalAddress || a1.isAnyLocalAddress || a0 == a1
        }
        if (conflict) Some(ConflictingPorts(addr0, addr1)) else None
      }

      port match {
        case Some(p) if !validPort(p) =>
          invalid(InvalidPort(p))
        case _ =>
          val allConflicts: Seq[ConflictingPorts] = others flatMap conflicts
          allConflicts match {
            case c :: cs => invalid(NonEmptyList[ConfigError](c, cs))
            case _ => valid(new Validated(this))
          }
      }
    }
  }

  class Validated(defaults: Defaults) {
    def addr = defaults.addr
  }
  */
  def validateServers(
    servers: Seq[ServerConfig],
    router: RouterConfig,
    previousServers: Seq[ServerConfig]
  ): Try[Seq[ServerParams]] = {
    Try.collect(servers.map(_.validated(router, previousServers)))
  }
}

// A convenience implementation for cases where the server type doesn't have any specialized configuration.
// TODO: This is prob not useful since we usually need to type-specialize on the req/rep types
case class BasicServerConfig(ip: Option[InetAddress], port: Option[Port]) extends ServerConfig {
  def validated(router: RouterConfig, prevServers: Seq[ServerConfig]): Try[ServerParams] =
    Return(BasicServerParams(Stack.Params.empty))
}

trait ServerParams extends ConfigParams

case class BasicServerParams(params: Stack.Params) extends ServerParams
