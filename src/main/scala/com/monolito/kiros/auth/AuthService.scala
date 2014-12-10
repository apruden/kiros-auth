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
import com.monolito.kiros.auth.model.Client
import scala.util.Success
import scala.util.Failure


class AuthServiceActor extends Actor with AuthService {

  def actorRefFactory = context

  def receive = runRoute(authRoutes)
}

case class Reader[C, A](g: C => A) {
  def apply(c: C) = g(c)
  def map[B](f: A => B): Reader[C, B] =
    (c:C) => f(g(c))
  def flatMap[B](f: A => Reader[C, B]): Reader[C, B] =
    (c:C) => f(g(c))(c)
}

object Reader {
  implicit def Reader[A,B](f: A => B): Reader[A,B] = Reader(f)

  def pure[A, C](a: A): Reader[C,A] =
    (c: C) => a
}

case class DBTOpt[C, R](g: Reader[C, Option[R]]) {
  def map[B](f: R => B): DBTOpt[C, B] = DBTOpt { Reader { g(_) map f } }

  def flatMap[B](f: R => DBTOpt[C, B]): DBTOpt[C, B] =
    DBTOpt {
      Reader { c =>
        (g(c) map f) match {
          case Some(r) => r.g(c)
          case None => None
        }
      }
    }
}

object DBTOpt {
  def pure[CTag, R](value : => Option[R]): DBTOpt[CTag, R] =
    DBTOpt { Reader { _ => value } }

  implicit def toDBT[CTag, R](db : Reader[CTag, Option[R]]): DBTOpt[CTag, R] =
    DBTOpt { db }

  implicit def fromDBT[CTag, R](dbto : DBTOpt[CTag, R]): Reader[CTag, Option[R]] =
   dbto.g
}

case class DBTFuture[C, R](g: Reader[C, Future[R]]) {
  def map[B](f: R => B): DBTFuture[C, B] = DBTFuture { Reader { g(_) map f } }

  def flatMap[B](f: R => DBTFuture[C, B]): DBTFuture[C, B] =
    DBTFuture {
      Reader { (c: C) => {
          val p: Promise[B] = Promise()
          val fut = g(c)

          fut.onComplete {
            case Success(x) => p.completeWith(f(x).g(c))
            case Failure(t) => p.failure(t)
          }

          p.future
        }
      }
    }
}

object DBTFuture {
  def pure[CTag, R](value: => Future[R]): DBTFuture[CTag, R] =
    DBTFuture { Reader { _ => value } }

  implicit def toDBTFut[CTag, R](db: Reader[CTag, Future[R]]): DBTFuture[CTag, R] =
    DBTFuture { db }

  implicit def fromDBTFut[CTag, R](dbto: DBTFuture[CTag, R]): Reader[CTag, Future[R]] =
    dbto.g
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

  def authorize(data: AuthorizeDto): Reader[ClientRepository, Future[Option[String]]] =
      for {
        dbo <- DBTFuture { toto("1") }
        d <- toto(dbo.get)
      } yield d

  def toto(id: String): Reader[ClientRepository, Future[Option[String]]] =
      for {
        res <- DBTFuture { (repo:ClientRepository) => {repo.get("b")} }
      } yield res
}