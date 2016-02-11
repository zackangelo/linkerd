package io.buoyant.linkerd.config

import com.twitter.finagle.Stack

trait ConfigParams {
  def params: Stack.Params
}
