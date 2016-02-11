package io.buoyant.linkerd.config

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.twitter.finagle.{Namer, Stack, Path}
import com.twitter.util.{Return, Try}

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
 * [[NamerConfig]] in `namers`.  _frizzle_ and _swizzle_ are
 * namer-specific options.  This namer refines names beginning with
 * `/i` (after this prefix has been stripped).
 */
// TODO: switch to using class names once we have fully replaced the existing system.
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "kind")
trait NamerConfig {
  import Try._
  // These are vars to allow subclasses to avoid the need to know about them
  var prefix: Option[Path] = None
  def kind: String
  def defaultPrefix: Option[Path] = None

  protected def params: Try[Stack.Params] = {
   /* prefix.foldLeft(Stack.Params.empty)(_ + _)
    def validatedPrefix: Try[Path] = prefix orElse defaultPrefix orThrow MissingPath*/
    Return(Stack.Params.empty)
  }

  def namer: Try[Namer]
}

trait NamerParams extends ConfigParams {
  def namer: Namer
}
abstract case class BasicNamerParams(params: Stack.Params) extends NamerParams
