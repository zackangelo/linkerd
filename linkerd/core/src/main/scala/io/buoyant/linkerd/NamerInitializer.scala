package io.buoyant.linkerd

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility
import com.fasterxml.jackson.annotation.{JsonProperty, JsonAutoDetect, JsonIgnore, JsonTypeInfo}
import com.fasterxml.jackson.databind.ObjectMapper
import com.twitter.finagle.{Namer, Path}

/**
 * Read a single namer configuration in the form:
 *
 * <pre>
 *   kind: io.l5d.izzle
 *   prefix: /i
 *   frizzle: dee
 *   swizzle: dah
 * </pre>
 *
 * In this example _io.l5d.izzle_ must be the _kind_ of a
 * [[NamerInitializer]] in `namers`.  _frizzle_ and _swizzle_ are
 * namer-specific options.  This namer refines names beginning with
 * `/i` (after this prefix has been stripped).
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "kind")
@JsonAutoDetect(fieldVisibility = Visibility.ANY)
abstract class NamerConfig {
  // This is a var because it allows us to add default parameters without
  // needing to update subclasses
  // This JsonProperty annotation is required for some mysterious reason.
  // Perhaps "prefix" has some special meaning to jackson?  This isn't needed
  // for other properties.......
  @JsonProperty("prefix")
  var prefix: Option[Path] = None

  @JsonIgnore
  def defaultPrefix: Path

  @JsonIgnore
  def getPrefix = prefix.getOrElse(defaultPrefix)

  /**
   * Construct a namer.
   */
  @JsonIgnore
  def newNamer(): Namer
}

abstract class NamerInitializer extends ConfigInitializer
