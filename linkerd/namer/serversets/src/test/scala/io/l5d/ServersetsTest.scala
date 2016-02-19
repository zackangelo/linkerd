package io.l5d

import io.buoyant.linkerd.NamerConfig
import io.buoyant.linkerd.config.Parser
import org.scalatest.FunSuite

class ServersetsTest extends FunSuite {

  def parse(yaml: String): ServersetsConfig = {
    val mapper = Parser.objectMapper(yaml)
    serversets.registerSubtypes(mapper)
    mapper.readValue[NamerConfig](yaml).asInstanceOf[ServersetsConfig]
  }

  test("zkHost list") {
    val yaml = """
kind: io.l5d.serversets
zkAddrs:
- host: foo
  port: 2181
- host: bar
  port: 2182
"""
    assert(parse(yaml).connectString == "foo:2181,bar:2182")
  }

  test("single zkHost") {
    val yaml = """
kind: io.l5d.serversets
zkAddrs:
- host: foo
  port: 2181
"""
    assert(parse(yaml).connectString == "foo:2181")
  }

  test("missing hostname") {
    val yaml = """
kind: io.l5d.serversets
zkAddrs:
- port: 2181
"""
    intercept[Exception](parse(yaml))
  }

  test("default port") {
    val yaml = """
kind: io.l5d.serversets
zkAddrs:
- host: foo
"""
    assert(parse(yaml).connectString == "foo:2181")
  }
}
