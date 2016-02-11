package io.l5d.experimental

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.twitter.finagle._
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.param.Label
import com.twitter.util.Try
import io.buoyant.k8s.{AuthFilter, EndpointsNamer, SetHostFilter}
import io.buoyant.k8s.v1.Api
import io.buoyant.linkerd.NamerConfig
import io.buoyant.linkerd.config.types.{ Port => TPort }
import io.buoyant.linkerd.{NamerInitializer, Parsing}
import io.l5d.experimental.k8s.AuthToken
import io.l5d.experimental.k8s.Tls.WithoutValidation
import scala.io.Source

/**
 * Supports namer configurations in the form:
 *
 * <pre>
 * namers:
 * - kind: io.l5d.experimental.k8s
 *   host: k8s-master.site.biz
 *   port: 80
 *   tls: false
 *   authTokenFile: ../auth.token
 * </pre>
 */
object k8s {

  /** The kubernetes master host; default: kubernetes.default.cluster.local */
  case class Host(host: String)
  implicit object Host extends Stack.Param[Host] {
    val default = Host("kubernetes.default.cluster.local")
  }

  /** The kubernetes master port; default: 443 (https) */
  implicit object Port extends Stack.Param[TPort] {
    val default = TPort(443)
  }

  /** Whether TLS is used to communicate with the master; default: true */
  case class Tls(enabled: Boolean)
  implicit object Tls extends Stack.Param[Tls] {
    val default = Tls(true)

    /** Whether hostname validation is performed with TLS. */
    case class WithoutValidation(enabled: Boolean)
    implicit object WithoutValidation extends Stack.Param[WithoutValidation] {
      val default = WithoutValidation(false)
    }
  }

  /**
   * The path to a file containing the k8s master's authorization token.
   * default: none
   */
  case class AuthToken(token: String) {
    def filter(): Filter[Request, Response, Request, Response] = token match {
      case "" => Filter.identity[Request, Response]
      case path => new AuthFilter(token)
    }
  }

  implicit object AuthToken extends Stack.Param[AuthToken] {
    // Kubernetes mounts a secrets volume with master authentication
    // tokens.  That's usually what we want.
    val default = AuthToken("")
  }

  object AuthTokenDeserializer extends StdDeserializer[AuthToken](classOf[AuthToken]) {
    override def deserialize(jp: JsonParser, ctxt: DeserializationContext): AuthToken = {
      val tokenFile = _parseString(jp, ctxt)
      AuthToken(Source.fromFile(tokenFile).mkString)
    }
  }

  val defaultParams = Stack.Params.empty +
    NamerInitializer.Prefix(Path.Utf8("io.l5d.k8s"))

  /**
   * Build a Namer backed by a Kubernetes master.
   */
  def newNamer() = {
    val k8s.Host(host) = params[k8s.Host]
    val k8s.Port(port) = params[k8s.Port]
    val setHost = new SetHostFilter(host, port)

    val client = (params[k8s.Tls], params[k8s.Tls.WithoutValidation]) match {
      case (k8s.Tls(false), _) => Http.client
      case (_, k8s.Tls.WithoutValidation(true)) => Http.client.withTlsWithoutValidation
      case _ => Http.client.withTls(setHost.host)
    }
  }
}



case class k8s(
    host: Option[String],
    port: Option[TPort],
    tls: Option[Boolean],
    tlsWithoutValidation: Option[Boolean],
    authTokenFile: Option[AuthToken])
  extends NamerConfig {
  import k8s._
  override def defaultParams = super.defaultParams ++ k8s.defaultParams
  override def initializerFactory = k8s.Initializer.apply
  override def params: Try[Stack.Params] = super.params map { ps =>
    Seq(
      host.map(Host(_)),
      port,
      tls.map(Tls(_)),
      tlsWithoutValidation.map(WithoutValidation(_)),
      authTokenFile)
      .flatten
      .foldLeft(ps)(_ + _)
  }
}

