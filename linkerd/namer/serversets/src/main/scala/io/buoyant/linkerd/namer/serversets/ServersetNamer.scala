// modified from com.twitter.serverset | (c) 2015 Twitter, Inc. | http://www.apache.org/licenses/LICENSE-2.0 */
package io.buoyant.linkerd.namer.serversets

import com.twitter.finagle._
import com.twitter.util.{Activity, Var}

/**
 * The serverset namer takes Paths of the form
 *
 * {{{
 * hosts/path...
 * }}}
 *
 * and returns a dynamic represention of the resolution of the path into a
 * tree of Names.
 *
 * The namer synthesizes nodes for each endpoint in the serverset.
 * Endpoint names are delimited by the ':' character. For example
 *
 * {{{
 * /$/io.l5d.serversets/discovery/prod/foo:http
 * }}}
 *
 * is the endpoint `http` of serverset `/discovery/prod/foo`
 *
 * Cribbed from https://github.com/twitter/finagle/blob/develop/finagle-serversets/src/main/scala/com/twitter/serverset.scala
 */
class ServersetNamer(zkHost: String) extends Namer {
  val idPrefix = Path.Utf8("$", "io.l5d.serversets")

  /** Resolve a resolver string to a Var[Addr]. */
  protected[this] def resolve(spec: String): Var[Addr] = Resolver.eval(spec) match {
    case Name.Bound(addr) => addr
    case _ => Var.value(Addr.Neg)
  }

  protected[this] def resolveServerset(hosts: String, path: String) =
    resolve(s"zk2!$hosts!$path")

  protected[this] def resolveServerset(hosts: String, path: String, endpoint: String) =
    resolve(s"zk2!$hosts!$path!$endpoint")

  /** Bind a name. */
  protected[this] def bind(path: Path): Option[Name.Bound] = {
    path match {
      case Path.Utf8(segments@_*) =>
        val addr = if (segments.nonEmpty && (segments.last contains ":")) {
          val Array(name, endpoint) = segments.last.split(":", 2)
          val zkPath = (segments.init :+ name).mkString("/", "/", "")
          resolveServerset(zkHost, zkPath, endpoint)
        } else {
          val zkPath = segments.mkString("/", "/", "")
          resolveServerset(zkHost, zkPath)
        }

        // Clients may depend on Name.Bound ids being Paths which resolve
        // back to the same Name.Bound
        val id = idPrefix ++ path
        Some(Name.Bound(addr, id))

      case _ => None
    }
  }

  // We have to involve a serverset roundtrip here to return a tree. We run the
  // risk of invalidating an otherwise valid tree when there is a bad serverset
  // on an Alt branch that would never be taken. A potential solution to this
  // conundrum is to introduce some form of lazy evaluation of name trees.
  def lookup(path: Path): Activity[NameTree[Name]] = bind(path) match {
    case Some(name) =>
      // We have to bind the name ourselves in order to know whether
      // it resolves negatively.
      Activity(name.addr map {
        case Addr.Bound(_, _) => Activity.Ok(NameTree.Leaf(name))
        case Addr.Neg => Activity.Ok(NameTree.Neg)
        case Addr.Pending => Activity.Pending
        case Addr.Failed(exc) => Activity.Failed(exc)
      })

    case None => Activity.value(NameTree.Neg)
  }
}
