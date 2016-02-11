package io.buoyant.linkerd

import com.fasterxml.jackson.core.JsonParser
import com.twitter.finagle.{Addr, Name, NameTree, Namer, Path, Stack}
import com.twitter.util.{Try, Activity, Var}

object TestNamer {
  case class Buh(buh: Boolean)
  implicit object Buh extends Stack.Param[Buh] {
    val default = Buh(false)
    val parser = Parsing.Param.Boolean("buh")(Buh(_))
  }

  val defaultParams = Stack.Params.empty +
    NamerInitializer.Prefix(Path.read("/foo"))
}

abstract class TestNamerConfig extends NamerConfig {
  import TestNamer._
  var buh: Option[Boolean] = None

  override def defaultParams = TestNamer.defaultParams
  override def params: Try[Stack.Params] = super.params.map { ps =>
    buh map { b => ps + Buh(b) } getOrElse ps
  }
  override def newInitializer(ps: Stack.Params) = new TestNamer(ps)
}

class TestNamer(val params: Stack.Params) extends NamerInitializer(params) {
  def this() = this(TestNamer.defaultParams)
  def withParams(ps: Stack.Params) = new TestNamer(ps)

  def newNamer() = new Namer {
    val buh = params[TestNamer.Buh].buh

    def lookup(path: Path): Activity[NameTree[Name]] = {
      val t = path match {
        case Path.Utf8("buh", _*) if !buh => NameTree.Neg
        case path =>
          val addr = Var.value(Addr.Pending)
          NameTree.Leaf(Name.Bound(addr, prefix, path))
      }
      Activity.value(t)
    }
  }
}
