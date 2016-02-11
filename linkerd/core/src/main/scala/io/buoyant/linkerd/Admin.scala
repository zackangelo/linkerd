package io.buoyant.linkerd

import com.twitter.finagle.Stack
import com.twitter.util.{Return, Try}
import io.buoyant.linkerd.config.types.Port

case class Admin(params: Stack.Params = Stack.Params.empty)

case class AdminConfig(port: Option[Port])  {
  import Admin._
  def validated: Admin = {
    Admin(port match {
      case Some(p) => Stack.Params.empty + AdminPort(p.port)
      case None => Stack.Params.empty
    })
  }
}

object Admin {
  case class AdminPort(port: Int)
  implicit object AdminPort extends Stack.Param[AdminPort] {
    override def default: AdminPort = AdminPort(9990)
  }
}
