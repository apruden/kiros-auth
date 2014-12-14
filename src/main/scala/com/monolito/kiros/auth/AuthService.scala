package com.monolito.kiros.auth

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

import org.slf4j.LoggerFactory

import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.MongoClientRepository
import com.monolito.kiros.auth.model._
import com.monolito.kiros.auth.model.Client

import akka.actor.Actor
import scalaz._
import scalaz.Scalaz._
import spray.http._
import spray.http.MediaTypes._
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing._

class AuthServiceActor extends Actor with AuthService {

  def actorRefFactory = context

  def receive = runRoute(authRoutes)
}

object AuthJsonProtocol extends DefaultJsonProtocol {

  implicit object ClientTypeJsonFormat extends RootJsonFormat[ClientType] {
    def write(c: ClientType) = JsString(c.name)
    def read(value: JsValue) = value match {
      case JsString("public") => PUBLIC
      case JsString("confidential") => CONFIDENTIAL
      case _ => deserializationError("Valid values are \"public\" and \"confidential\"")
    }
  }

  implicit val ClientFormat = jsonFormat3(Client)
}

trait AuthService extends HttpService {
  import AuthJsonProtocol._

  val logger = LoggerFactory.getLogger(this.getClass)

  type #>[E, A] = Kleisli[Future, E, A]

  object ReaderFutureT extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Future[B]): A #> B = Kleisli(f)
    def pure[A, B](r: => Future[B]): A #> B = Kleisli(_ => r)
  }

  object ReaderOptionT extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Option[B]): A =?> B = kleisli(f)
    def pure[A, B](r: => Option[B]): A =?> B = kleisli(_ => r)
  }

  implicit def toReader[C, R](f: C => R) = Reader(f)
  implicit def toReaderOptionT[C, R](f: Reader[C, Option[R]]) = ReaderOptionT(f)
  implicit def toReaderFutureT[C, R](f: Reader[C, Future[R]]) = ReaderFutureT(f)

  implicit val clientTypeUnmarshaller =
    Unmarshaller[ClientType](MediaTypes.`text/plain`) {
      case HttpEntity.NonEmpty(contentType, data) => {
        data.asString match {
          case "public" => PUBLIC
          case "confidential" => CONFIDENTIAL
          case _ => throw new Exception("toto")
        }
      }
      case _ => throw new Exception("invalid value")
    }

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
          entity(as[AuthorizeRequest]) { //responseType in ['code', 'token']
            (dto) =>
              complete {
                authorize(dto)(new MongoClientRepository)
              }
          }
        }
      }
    } ~ path("token") { //get
      post {
        decompressRequest() {
          entity(as[AccessTokenRequest]) {
            (dto) => ???
          }
        }
      }
    } ~ path("clients") {
      get {
        rejectEmptyResponse {
          parameters('id) {
            id =>
              complete {
                getClient(id)(new MongoClientRepository)
              } // return clientId and clientSecret if the client is confidential
          }
        }
      } ~
        (post | put) {
          formFields('name, 'client_type.as[ClientType], 'redirect_uri).as(Client)(
            client => onSuccess(saveOrUpdateClient(client)(new MongoClientRepository)) {
              _ => complete("OK")
            } // return clientId and clientSecret if the client is confidential
            )
        } ~ path(Segment) { name =>
          delete {
            onSuccess(deleteClient(name)(new MongoClientRepository)) {
              _ => complete("OK")
            }
          }
        }
    }

  def authorize(data: AuthorizeRequest): ClientRepository #> Option[String] = ???

  def getClient(id: String): ClientRepository #> Option[Client] =
    ReaderFutureT { (r: ClientRepository) => r.get(id) }

  def saveOrUpdateClient(client: Client): ClientRepository #> Try[Unit] =
    for {
      c <- ReaderFutureT { (r: ClientRepository) => r.save(client) }
    } yield c

  def deleteClient(id: String): ClientRepository #> Try[Unit] =
    ReaderFutureT { (r: ClientRepository) => r.del(id) }
}