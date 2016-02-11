package io.buoyant.linkerd

import com.fasterxml.jackson.annotation.JsonTypeInfo
import com.fasterxml.jackson.core.{JsonParser, JsonToken, TreeNode}
import com.twitter.finagle.{Namer, Path, Stack}
import com.twitter.util.{Return, Throw, Try}

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
@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "kind")
abstract class NamerConfig {
  // This is a var because it allows us to add default parameters without
  // needing to update subclasses
  var prefix: Option[Path] = None

  def defaultParams: Stack.Params = Stack.Params.empty

  /** Configuration state. */
  protected def params: Try[Stack.Params] = {
    prefix match {
      case Some(path) if path.isEmpty => Throw(new IllegalArgumentException("namer prefix must not be empty"))
      case Some(path) => Return(defaultParams + NamerInitializer.Prefix(path))
      case None => Return(defaultParams)
    }
  }

  protected def initializerFactory: Stack.Params => NamerInitializer

  def initializer: Try[NamerInitializer] = params map initializerFactory
}

abstract class NamerInitializer(params: Stack.Params) {
  def prefix: Path = params[NamerInitializer.Prefix].path

  /**
   * Construct a namer.
   */
  def newNamer(): Namer
}

object NamerInitializer {
  /**
   * A configuration parameter that indicates the prefix of names that
   * should be refined through a given namer.  For example, if a
   * NamerInitializer is configured with the prefix `/pfx`, then a
   * name like `/pfx/mule/variations` would cause the name
   * `/mule/variations` to be resolved through the resulting Namer.
   */
  case class Prefix(path: Path)
  implicit object Prefix extends Stack.Param[Prefix] {
    val default = Prefix(Path.empty)
  }
}
