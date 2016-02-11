package io.buoyant.linkerd.config

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.{DeserializationContext, ObjectMapper}
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.jsontype.NamedType
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.twitter.finagle.util.LoadService
import com.twitter.util.Try
import scala.reflect.ClassTag
import scala.reflect.classTag
import Parser.jClass



/*
 * NOTE: Another option, rather than using NamedType, would be to use @JsonTypeName annotations on the relevant classes.
 */
abstract class NamedConfig[T: ClassTag](name: String) {
  def namedType: NamedType = new NamedType(classTag[T].runtimeClass, name)
}

abstract class NamedRouterConfig[T <: RouterConfig : ClassTag](name: String) extends NamedConfig[T](name)
abstract class NamedNamerConfig[T <: NamerConfig : ClassTag](name: String) extends NamedConfig[T](name)

abstract class ConfigDeserializer[T: ClassTag] extends StdDeserializer[T](jClass[T]) {
  def register(module: SimpleModule): SimpleModule = module.addDeserializer(jClass[T], this)
  protected def catchMappingException(ctxt: DeserializationContext)(t: => T): T =
    try t catch { case arg: IllegalArgumentException => throw ctxt.mappingException(arg.getMessage)}
}

/**
 *
 * @param parsedConfig A representation of the configuration file as it was provided, without any defaults applied.
 *                     Will be None if we were unable to parse the file at all due to a syntax error.
 * @param validatedConfig Either a list of [[ConfigError]]s representing problems validating the configuration, or a
 *               [[LinkerParams]] object which has been validated to initialize a linker.
 *
 */
case class ParseResult(
  parsedConfig: Try[LinkerConfig],
  validatedConfig: Try[LinkerParams]
)

object Parser {
  private[config] def jClass[T: ClassTag]: Class[T] = classTag[T].runtimeClass.asInstanceOf[Class[T]]

  def apply(s: String): ParseResult = {
    val baseCfg = Try { objectMapper(s).readValue[LinkerConfig](s) }

    ParseResult(
      baseCfg,
      baseCfg.flatMap(_.validated)
    )
  }

  private[this] def peekJsonObject(s: String): Boolean =
    s.dropWhile(_.isWhitespace).headOption == Some('{')

  /**
   * Load a Json or Yaml parser, depending on whether the content appears to be Json.
    * We expose this publicly for testing purposes (to allow easy parsing of config subtrees) at the moment.
   */
  def objectMapper(config: String): ObjectMapper with ScalaObjectMapper = {
    val factory = if (peekJsonObject(config)) new JsonFactory() else new YAMLFactory()
    val customTypes = LoadService[ConfigDeserializer[_]]().foldLeft(new SimpleModule("linkerd custom types")) {
      (module, d) => { d.register(module) }
    }

    val mapper = new ObjectMapper(factory) with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
    mapper.registerModule(customTypes)
    def subtypes[T <: NamedConfig[_] : ClassTag](): Seq[NamedType] = {
      val types: List[NamedType] = LoadService[T]().map(_.namedType).toList
      def findConflict(ts: List[NamedType], seen: Map[String, NamedType] = Map.empty): Option[(NamedType, NamedType)] = {
        ts match {
          case head :: tail => seen.get(head.getName) match {
            case Some(conflict) => Some((conflict, head))
            case None => findConflict(tail, seen + (head.getName -> head))
          }
          case Nil => None
        }
      }
      findConflict(types) match {
        case Some((t0, t1)) => throw ConflictingSubtypes(t0, t1)
        case None => types
      }
    }
    val routerTypes = subtypes[NamedRouterConfig[_]]()
    val namerTypes = subtypes[NamedNamerConfig[_]]()
    mapper.registerSubtypes(routerTypes ++ namerTypes: _*)
    mapper
  }
}

