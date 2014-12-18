package com.monolito.kiros.auth

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.MongoClientRepository
import com.monolito.kiros.auth.model._
import akka.actor.Actor
import spray.http._
import spray.http.MediaTypes._
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing._
import com.monolito.kiros.auth.data.UserRepository
import com.monolito.kiros.auth.data.MongoUserRepository
import scalaz._
import Scalaz._
import scalaz.OptionT._

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

  implicit val clientFormat = jsonFormat3(Client)

  implicit val accessTokenFormat = jsonFormat4(AccessToken)
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
          }) // return clientId and clientSecret if the client is confidential
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
          parameters('client_id, 'scope, 'state, 'redirect_uri, 'response_type) {
            (client_id, scope, state, redirect_uri, response_type) => complete(html.grant_form(client_id, scope, state).toString)
          }
        }
      } ~ post {
        entity(as[AuthorizeRequest]) { //responseType in ['code', 'token']
          (dto) =>
            onSuccess(authorize(dto)(AppContext(new MongoClientRepository, new MongoUserRepository))) {
              r =>
                r match {
                  case scala.util.Success(t) => complete(t)
                  case scala.util.Failure(ex) => complete(ex)
                }
            }
        }
      }
    } ~ path("token") {
      post {
        entity(as[AccessTokenRequest]) {
          (dto) => ???
        }
      }
    }

  def authorize(data: AuthorizeRequest): AppContext #> Try[AccessToken] =
    ReaderFutureT { ctx =>
      {
        val a = (for {
          d <- optionT(ctx.clients.get(data.clientId))
          c <- optionT(ctx.users.get(data.username.get))
          if c.password == Utils.getHmac(data.password.get)
        } yield c).run

        for {
          x <- a
          c <- Future.successful(x match {
            case Some(y) => Some(y)
            case None => Some(User(java.util.UUID.randomUUID.toString, data.username.get, data.password.get))
          })
          r <- ctx.users.save(c.get)
        } yield Try(buildAccessToken(c.get, data))
      }
    }

  def buildAccessToken(u: User, data:AuthorizeRequest): AccessToken =
    AccessToken(s"${u.userId}:${data.scope}:${System.currentTimeMillis}|${Utils.getHmac(u.userId + data.scope + System.currentTimeMillis)}", "token", data.scope, data.state)

  def createUser(username: String, password: String): AppContext #> Try[User] = ???
}