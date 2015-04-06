package com.monolito.kiros.auth

import akka.actor.{ ActorSystem, Props }
import akka.io.IO
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import spray.io.ServerSSLEngineProvider

object Boot extends App { //with AuthSslConfiguration {
  import com.monolito.kiros.auth.conf
  import data.EsRepository._

  implicit val system = ActorSystem("on-spray-can")

  val service = system.actorOf(Props[AuthServiceActor], "kiros-auth-service")

  implicit val timeout = Timeout(5.seconds)

  tryCreateIndex()

  IO(Http) ? Http.Bind(service, interface = conf.getString("kiros.auth.host"), port = conf.getInt("kiros.auth.port"))
}
