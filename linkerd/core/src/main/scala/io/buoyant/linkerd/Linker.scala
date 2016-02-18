package io.buoyant.linkerd

import com.twitter.finagle.Stack.Param
import com.twitter.finagle.buoyant.DstBindingFactory
import com.twitter.finagle.naming.NameInterpreter
import com.twitter.finagle.util.LoadService
import com.twitter.finagle.{Namer, Path, Stack}
import io.buoyant.linkerd.config.Parser
import io.buoyant.linkerd.ProtocolInitializer.ParamsMaybeWith

/**
 * Represents the total configuration of a Linkerd process.
 *
 * `params` are default router params defined in the top-level of a
 * linker configuration, and are used when reading [[Router Routers]].
 */
trait Linker {
  def routers: Seq[Router]
  def admin: Admin

  def configured[T: Stack.Param](t: T): Linker
}

object Linker {

  def load(config: String, configInitializers: Seq[ConfigInitializer]): Linker = {
    val mapper = Parser.objectMapper(config)
    for (ci <- configInitializers) ci.registerSubtypes(mapper)
    // TODO: Store the LinkerConfig so that it can be serialized out later
    mapper.readValue[LinkerConfig](config).mk
  }

  def load(config: String): Linker = {
    val protocols = LoadService[ProtocolInitializer]
    val namers = LoadService[NamerInitializer]
    val clientTls = LoadService[TlsClientInitializer]
    load(config, protocols ++ namers ++ clientTls)
  }

  case class LinkerConfig(namers: Option[Seq[NamerConfig]], routers: Seq[RouterConfig], admin: Option[Admin]) {
    def mk: Linker = {
      val namersParam = Stack.Params.empty.maybeWith(namers.map(nameInterpreter).map(DstBindingFactory.Namer(_)))

      // TODO: validate at least one router
      // TODO: validate no router names conflict
      // TODO: validate no server sockets conflict
      new Impl(routers.map(_.router(namersParam)), admin.getOrElse(Admin()))
    }
  }

  def nameInterpreter(namers: Seq[NamerConfig]): NameInterpreter =
    Interpreter(namers.map { cfg =>
      cfg.getPrefix -> cfg.newNamer()
    })

  /**
   * Private concrete implementation, to help protect compatibility if
   * the Linker api is extended.
   */
  private case class Impl(
    routers: Seq[Router],
    admin: Admin
  ) extends Linker {
    override def configured[T: Param](t: T) =
      copy(routers = routers.map(_.configured(t)))
  }
}
