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

import spray.http.{HttpMethods, HttpMethod, HttpResponse, AllOrigins}
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import spray.routing.authentication._
import shapeless._

trait CORSSupport {
  this: HttpService =>
  private val allowOriginHeader = `Access-Control-Allow-Origin`(SomeOrigins(List(HttpOrigin("http" , Host("localhost",9000)))))
  private val optionsCorsHeaders = List(
    `Access-Control-Allow-Headers`("Origin, X-Requested-With, Content-Type, Accept, Accept-Encoding, Accept-Language, Host, Referer, User-Agent, Authorization"),
    `Access-Control-Max-Age`(1728000))

  def cors[T]: Directive0 = mapRequestContext { ctx => ctx.withRouteResponseHandling({
          case Rejected(x) if (ctx.request.method.equals(HttpMethods.OPTIONS) && !x.filter(_.isInstanceOf[MethodRejection]).isEmpty) => {
          val allowedMethods: List[HttpMethod] = x.filter(_.isInstanceOf[MethodRejection]).map(rejection=> {
              rejection.asInstanceOf[MethodRejection].supported
              })
          ctx.complete(HttpResponse().withHeaders(
                  `Access-Control-Allow-Methods`(OPTIONS, allowedMethods :_*) ::  allowOriginHeader ::
                  optionsCorsHeaders
                  ))
          }
          })
    .withHttpResponseHeadersMapped { headers =>
                allowOriginHeader :: headers
            }
  }
}

class OAuth2HttpAuthenticator[U](val realm: String, val oauth2Authenticator: OAuth2Authenticator[U])(implicit val executionContext: ExecutionContext)
extends HttpAuthenticator[U] {

  def authenticate(credentials: Option[HttpCredentials], ctx: RequestContext) =
    oauth2Authenticator {
      credentials.flatMap {
        case OAuth2BearerToken(token) => Some(token)
        case _ => None
      }
    }

  def getChallengeHeaders(httpRequest: HttpRequest) =
    `WWW-Authenticate`(HttpChallenge(scheme = "Bearer", realm = realm, params = Map.empty)) :: Nil

}

case class OAuthCred(id: String, scopes:List[String], expire: Long) {
  def anyScope(requiredScopes: List[String]): Boolean = !scopes.intersect(requiredScopes).isEmpty
}

object OAuth2Auth {
  def apply[T](authenticator: OAuth2Authenticator[T], realm: String)(implicit ec: ExecutionContext) =
    new OAuth2HttpAuthenticator[T](realm, authenticator)
}


class AuthServiceActor extends Actor with AuthService with ClientService  {
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

  implicit object UserJsonFormat extends RootJsonFormat[User] {
    def write(u: User) = JsObject ("username" -> JsString(u.username), "userId" -> JsString(u.userId))
    def read(value: JsValue) = value.asJsObject.getFields("username", "userId") match {
      case Seq(JsString(uname), JsString(id)) => User(uname, id, "")
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

trait AuthService extends HttpService with CORSSupport {
  import AuthJsonProtocol._
  val authenticated: Directive1[OAuthCred] = authenticate(OAuth2Auth(validateToken, "auth"))

  val authorized: List[String] => Directive1[OAuthCred] = (scope:List[String]) => authenticated.hflatMap {
    case c :: HNil =>  {
      if (c.anyScope(scope)) provide(c) else reject
    }
    case _ => reject
  }

  val authRoutes =
    pathSingleSlash {
      get {
        respondWithMediaType(`text/html`)(complete(html.index().toString))
      }
    } ~
    path("authorize") { //authorize third-party application. should display an authorize form (web-application)
      get {
        respondWithMediaType(`text/html`) {
          parameters('client_id, 'scope, 'state, 'redirect_uri, 'response_type) {
            (client_id, scope, state, redirect_uri, response_type) => complete(html.grant_form(client_id, scope, state, redirect_uri).toString)
          }
        }
      } ~
      post {
        entity(as[AuthorizeRequest]) { //responseType in ['code', 'token']
          dto =>
            onSuccess(authorize(dto)(AppContext(new EsClientRepository, new EsUserRepository))) {
              r =>
                r match {
                  case scala.util.Success(t) => redirect(s"${t._1}#?${t._2.redirectString}", StatusCodes.SeeOther)
                  case scala.util.Failure(ex) => complete(ex)
                }
            }
        }
      }
    } ~
    path("token") {
      post {
        entity(as[AccessTokenRequest]) {
          (dto) => ???
        }
      }
    } ~
    cors { 
      path("me") {
        get {
          authorized(List("auth")) {
            cred => complete(getUser(cred)(AppContext(new EsClientRepository, new EsUserRepository)))
          }
        }
      }
    }

  def authorize(data: AuthorizeRequest): AppContext #> Try[(String, AccessToken)] = {
    /*if (data.formToken != Utils.getHmac(data.redirectUri.get))
      return ReaderTFuture.pure { Future.failed(new Exception("Invalid form data"))
      }*/

    ReaderTFuture { ctx =>
      {
        val a = (for {
          u <- optionT(ctx.users.findByUsername(data.username.get))
        } yield u).run

        for {
          x <- a
          c <- Future.successful(x match {
            case Some(y) => {
              //TODO: validate password
              Some(y)
            }
            case None => Some(User(java.util.UUID.randomUUID.toString, data.username.get, data.password.get)) //TODO: hash password
          })
          r <- ctx.users.save(c.get)
        } yield Try((data.redirectUri.get, buildAccessToken(c.get, data)))
      }
    }
  }

  def getUser(cred: OAuthCred): AppContext #> Option[User] = {
    println("cred " + cred.id)
    ReaderTFuture (
      ctx => ctx.users.find(cred.id)
    )
  }

  def buildAccessToken(u: User, data: AuthorizeRequest) = {
    import java.util.Base64
    val token = new String(Base64.getEncoder.encode(s"${u.userId}:${data.scope}:${System.currentTimeMillis + 1800000}|${Utils.getHmac(u.userId + data.scope + (System.currentTimeMillis + 1800000))}".getBytes), "UTF-8")
    AccessToken(token, "token", data.scope, data.state)
  }

}
