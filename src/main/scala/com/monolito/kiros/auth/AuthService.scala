package com.monolito.kiros.auth

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.MongoClientRepository
import com.monolito.kiros.auth.model._
import com.monolito.kiros.auth.model.Client

import akka.actor.Actor
import spray.http._
import spray.http.MediaTypes._
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing._

class AuthServiceActor extends Actor with AuthService with ClientService {

  def actorRefFactory = context

  def receive = runRoute(authRoutes ~ clientRoutes)
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

trait ClientService extends HttpService {
  import AuthJsonProtocol._

  implicit val clientTypeUnmarshaller =
    Unmarshaller[ClientType](MediaTypes.`text/plain`) {
      case HttpEntity.NonEmpty(contentType, data) => {
        data.asString match {
          case "public" => PUBLIC
          case "confidential" => CONFIDENTIAL
          case _ => throw new Exception("Invalid client type")
        }
      }
      case _ => throw new Exception("Invalid value")
    }

  val clientRoutes = path("clients") {
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

  def getClient(id: String): ClientRepository #> Option[Client] =
    ReaderFutureT { (r: ClientRepository) => r.get(id) }

  def saveOrUpdateClient(client: Client): ClientRepository #> Try[Unit] =
    for {
      c <- ReaderFutureT { (r: ClientRepository) => r.save(client) }
    } yield c

  def deleteClient(id: String): ClientRepository #> Try[Unit] =
    ReaderFutureT { (r: ClientRepository) => r.del(id) }
}

trait AuthService extends HttpService {
  import AuthJsonProtocol._

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
      get {
        respondWithMediaType(`text/html`) {
          complete(html.index().toString)
        }
      } ~ post {
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
    }

  def authorize(data: AuthorizeRequest): ClientRepository #> Option[String] = ???

}