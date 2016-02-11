package io.buoyant.linkerd

import com.fasterxml.jackson.databind.jsontype.NamedType
import com.twitter.finagle.{Dtab, Name, NameTree, Namer, Path, Stack}
import org.scalatest.FunSuite

class booNamer extends TestNamerConfig {
 override def defaultParams = super.defaultParams + NamerInitializer.Prefix(Path.read("/boo"))
}

class booUrnsNamer extends TestNamerConfig {
  override def defaultParams = super.defaultParams + NamerInitializer.Prefix(Path.read("/boo/urns"))
}

class NamerInitializersTest extends FunSuite {

  val mapper = Yaml.objectMapper
  NamerInitializers.register(mapper, Seq(new NamedType(classOf[booNamer]), new NamedType(classOf[booUrnsNamer])))

  test("namers evaluated bottom-up") {
    val path = Path.read("/boo/urns")

    val booYaml =
      """|- kind: io.buoyant.linkerd.booUrnsNamer
         |- kind: io.buoyant.linkerd.booNamer
         |""".stripMargin
    val configs = mapper.readValue[Seq[NamerConfig]](booYaml)
    val initializers = configs.map(_.initializer.get)
    NamerInitializers.read(initializers).bind(Dtab.empty, path).sample() match {
      case NameTree.Leaf(bound: Name.Bound) =>
        assert(bound.id == Path.read("/boo"))
        assert(bound.path == Path.read("/urns"))
      case tree => fail(s"unexpected result: $tree")
    }

    val booUrnsYaml =
      """|- kind: io.buoyant.linkerd.booNamer
         |- kind: io.buoyant.linkerd.booUrnsNamer
         |""".stripMargin

    val urnsConfigs = mapper.readValue[Seq[NamerConfig]](booUrnsYaml)
    val urnsInitializers = configs.map(_.initializer.get)
    NamerInitializers.read(urnsInitializers).bind(Dtab.empty, path).sample() match {
      case NameTree.Leaf(bound: Name.Bound) =>
        assert(bound.id == Path.read("/boo/urns"))
        assert(bound.path == Path.empty)
      case tree => fail(s"unexpected result: $tree")
    }
  }
}
