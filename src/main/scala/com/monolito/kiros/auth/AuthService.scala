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
import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.MongoClientRepository


class AuthServiceActor extends Actor with AuthService {

  def actorRefFactory = context

  def receive = runRoute(authRoutes)
}


case class Middleware[C, A](g: C => A) {
  def apply(c: C) = g(c)
  def map[B](f: A => B): Middleware[C, B] =
    (c:C) => f(g(c))
  def flatMap[B](f: A => Middleware[C, B]): Middleware[C, B] =
    (c:C) => f(g(c))(c)
}

object Middleware {
  implicit def middleware[A,B](f: A => B): Middleware[A,B] = Middleware(f)

  def pure[A, C](a: A): Middleware[C,A] =
    (c: C) => a
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
                complete { 
                  authorize(dto)(new MongoClientRepository)
                  }
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

  def authorize(data: AuthorizeDto): Middleware[ClientRepository, Future[String]] =
    (repo: ClientRepository) => {
      Future { "test" }
    }
}