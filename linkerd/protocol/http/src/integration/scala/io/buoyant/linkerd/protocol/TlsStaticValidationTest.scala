package io.buoyant.linkerd
package protocol

import com.twitter.conversions.time._
import com.twitter.finagle.Failure
import com.twitter.finagle.http.Request
import io.buoyant.linkerd.protocol.TlsUtils._
import io.buoyant.test.Awaits
import io.l5d.clientTls.StaticInitializer
import org.scalatest.FunSuite

class TlsStaticValidationTest extends FunSuite with Awaits {

  override val defaultWait = 2.seconds

  test("tls router + plain upstream with static validation") {
    withCerts("linkerd") { certs =>
      val dog = Downstream.constTls("dogs", "woof", certs.serviceCerts("linkerd").cert,
        certs.serviceCerts("linkerd").key)
      try {
        val linkerConfig =
          s"""
             |routers:
             |- protocol: http
             |  baseDtab: |
             |    /p/dog => /$$/inet/127.1/${dog.port} ;
             |    /http/1.1/GET/clifford => /p/dog ;
             |  servers:
             |  - port: 0
             |  client:
             |    tls:
             |      kind: io.l5d.clientTls.static
             |      commonName: linkerd
             |      caCertPath: ${certs.caCert.getPath}
             |""".
            stripMargin
        val linker = Linker.load(linkerConfig, Seq(HttpInitializer, StaticInitializer))
        val router = linker.routers.head.initialize()
        try {
          val server = router.servers.head.serve()
          try {
            val client = upstream(server)
            try {
              val rsp = {
                val req = Request()
                req.host = "clifford"
                await(client(req))
              }
              assert(rsp.contentString == "woof")

            } finally (await(client.close()))
          } finally (await(server.close()))
        } finally (await(router.close()))
      } finally (await(dog.server.close()))
    }
  }

  test("tls router + plain upstream with static validation and incorrect common name") {
    withCerts("linkerd") { certs =>
      val dog = Downstream.constTls("dogs", "woof", certs.serviceCerts("linkerd").cert,
        certs.serviceCerts("linkerd").key)
      try {
        val linkerConfig =
          s"""
             |routers:
             |- protocol: http
             |  baseDtab: |
             |    /p/dog => /$$/inet/127.1/${dog.port} ;
             |    /http/1.1/GET/clifford => /p/dog ;
             |  servers:
             |  - port: 0
             |  client:
             |    tls:
             |      kind: io.l5d.clientTls.static
             |      commonName: wrong
             |      caCertPath: ${certs.caCert.getPath}
             |""".
            stripMargin
        val linker = Linker.load(linkerConfig, Seq(HttpInitializer, StaticInitializer))
        val router = linker.routers.head.initialize()
        try {
          val server = router.servers.head.serve()
          try {
            val client = upstream(server)
            try {
              val rsp = {
                val req = Request()
                req.host = "clifford"
                intercept[Failure](await(client(req)))
              }
            } finally (await(client.close()))
          } finally (await(server.close()))
        } finally (await(router.close()))
      } finally (await(dog.server.close()))
    }
  }
}
