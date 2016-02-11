package io.buoyant.linkerd

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import io.buoyant.linkerd.config.Parser

/**
 * Parser utilities
 *
 * We keep this in the test so that it's easy to write config blocks
 * without incurring a dependency on databind or dataformat-yaml from
 * linkerd-core.
 */
object Yaml {
  private[this] val factory = new YAMLFactory(new ObjectMapper)
  def apply(str: String): JsonParser = {
    val p = factory.createParser(str)
    p.nextToken()
    p
  }

  def objectMapper: ObjectMapper with ScalaObjectMapper = {
    val factory = new YAMLFactory()
    val mapper = new ObjectMapper(factory) with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper
  }
}
