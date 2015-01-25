package com.monolito.kiros.auth

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.EsClientRepository
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
import com.monolito.kiros.auth.data.EsUserRepository
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

  implicit val clientFormat = jsonFormat4(Client)

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
              getClient(id)(new EsClientRepository)
            } // return clientId and clientSecret if the client is confidential
        }
      }
    } ~
      (post | put) {
        formFields('client_id, 'name, 'client_type.as[ClientType], 'redirect_uri).as(Client)(
          client => onSuccess(saveOrUpdateClient(client)(new EsClientRepository)) {
            _ => complete("OK")
          }) // return clientId and clientSecret if the client is confidential
      } ~ path(Segment) { name =>
        delete {
          onSuccess(deleteClient(name)(new EsClientRepository)) {
            _ => complete("OK")
          }
        }
      }
  }

  def getClient(id: String): ClientRepository #> Option[Client] =
    ReaderTFuture(ctx => ctx.find(id))

  def saveOrUpdateClient(client: Client): ClientRepository #> Try[Unit] =
    for {
      c <- ReaderTFuture { (r: ClientRepository) => r.save(client) }
    } yield c

  def deleteClient(id: String): ClientRepository #> Try[Unit] =
    ReaderTFuture { (r: ClientRepository) => r.del(id) }
}

trait AuthService extends HttpService {
  import AuthJsonProtocol._

  val authRoutes =
    pathSingleSlash {
      get {
        respondWithMediaType(`text/html`)(complete(html.index().toString))
      }
    } ~ path("authorize") { //authorize third-party application. should display an authorize form (web-application)
      get {
        respondWithMediaType(`text/html`) {
          parameters('client_id, 'scope, 'state, 'redirect_uri, 'response_type) {
            (client_id, scope, state, redirect_uri, response_type) => complete(html.grant_form(client_id, scope, state, redirect_uri).toString)
          }
        }
      } ~ post {
        entity(as[AuthorizeRequest]) { //responseType in ['code', 'token']
          dto =>
            onSuccess(authorize(dto)(AppContext(new EsClientRepository, new EsUserRepository))) {
              r =>
                r match {
                  case scala.util.Success(t) => redirect(s"${t._1}#${t._2.redirectString}", StatusCodes.TemporaryRedirect)
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
    } ~ path("me") {
      get {
        complete ("OK")
      }
    }

  def authorize(data: AuthorizeRequest): AppContext #> Try[(String, AccessToken)] = {
    /*if (data.formToken != Utils.getHmac(data.redirectUri.get))
      return ReaderTFuture.pure { Future.failed(new Exception("Invalid form data"))
      }*/

    ReaderTFuture { ctx =>
      {
        val a = (for {
          u <- optionT(ctx.users.find(data.username.get))
        } yield u).run

        for {
          x <- a
          c <- Future.successful(x match {
            case Some(y) => Some(y)
            case None => Some(User(java.util.UUID.randomUUID.toString, data.username.get, data.password.get))
          })
          r <- ctx.users.save(c.get)
        } yield Try((data.redirectUri.get, buildAccessToken(c.get, data)))
      }
    }
  }

  def buildAccessToken(u: User, data: AuthorizeRequest) =
    AccessToken(s"${u.username}:${u.userId}:${data.scope}:${System.currentTimeMillis}|${Utils.getHmac(u.userId + data.scope + System.currentTimeMillis)}", "token", data.scope, data.state)

}
