package com.monolito.kiros.auth

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.RouteResult.route2HandlerFlow
import com.typesafe.config.ConfigFactory


object Boot extends App with AuthService {
  import com.monolito.kiros.auth.conf
  import data.EsRepository._

  val (host, port) = (conf.getString("kiros.auth.host"), conf.getInt("kiros.auth.port"))
  val bindingFuture = Http().bindAndHandle(handler=authRoutes, host, port) //, serverContext)
  println(s"Server online at http://$host:$port/ ...")
  sys.addShutdownHook(() => bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate()))
}
