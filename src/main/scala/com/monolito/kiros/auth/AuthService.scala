package com.monolito.kiros.auth

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._

class AuthServiceActor extends Actor with AuthService {

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}

trait AuthService extends HttpService {

  val myRoute =
    path("") {
      get {
        respondWithMediaType(`text/html`) {
          complete {
            <html>
              <body>
                <h1>Hello <i>xoipos-auth</i>!</h1>
              </body>
            </html>
          }
        }
      }
    }
}