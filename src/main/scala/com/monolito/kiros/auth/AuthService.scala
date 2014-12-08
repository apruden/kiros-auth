package com.monolito.kiros.auth

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._
import scala.concurrent._
import ExecutionContext.Implicits.global
import spray.json._
import DefaultJsonProtocol._
import spray.httpx.SprayJsonSupport._

class AuthServiceActor extends Actor with AuthService {

  def actorRefFactory = context

  def receive = runRoute(authRoutes)
}

trait AuthService extends HttpService {

  val authRoutes =
    pathSingleSlash {
      get {
        respondWithMediaType(`text/html`) {
          complete {
            <html>
              <body>
                <h1>Hello <i>kiros-auth</i>!</h1>
              </body>
            </html>
          }
        }
      }
    } ~ path("authorize") { //authorize third-party application. should display an authorize form (web-application)
      post {
        decompressRequest() {
          entity(as[AuthorizeDto]) { //responseType in ['code', 'token']
            (dto) =>
              detach() {
                complete { authorize(dto) }
              }
          }
        }
      }
    } ~ path("token") { //get
      post {
        decompressRequest() {
          entity(as[TokenDto]) {
            (dto) => ???
          }
        }
      }
    } ~ path("clients") {
      post {
        decompressRequest() {
          formFields('clientType?, 'redirectUri?) { // clientType in ['confidential', 'public']
            (p1, p2) => ??? // return clientId and clientSecret if the client is confidential
          }
        }
      }
    }

  def authorize(data: AuthorizeDto): Future[Either[String, Map[String, String]]] = {
    Future { Left(data.clientId) }
  }
}