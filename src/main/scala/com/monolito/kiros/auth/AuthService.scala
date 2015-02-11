package com.monolito.kiros.auth

import akka.actor.Actor
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import spray.http._
import spray.http.MediaTypes._
import spray.httpx.unmarshalling._
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.routing._
import com.monolito.kiros.auth.data.UserRepository
import com.monolito.kiros.auth.data.EsUserRepository
import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.EsClientRepository
import com.monolito.kiros.auth.model._
import scalaz._
import Scalaz._
import scalaz.OptionT._

import spray.http.{HttpMethods, HttpMethod, HttpResponse, AllOrigins}
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import spray.routing.authentication._

import SprayJsonSupport._

import java.util.Hashtable
import javax.naming.{ Context, NamingException, NamingEnumeration }
import javax.naming.ldap.InitialLdapContext
import javax.naming.directory.{ SearchControls, SearchResult, Attribute }
import scala.collection.JavaConverters._


class AuthServiceActor extends Actor with AuthService with ClientService  {
  import CustomRejectionHandler._

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
            onSuccess(getClient(id)(new EsClientRepository)) {
              c => complete(c.get) // return clientId and clientSecret if the client is confidential
            }
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
    ReaderTFuture { clients => clients.find(id) }

  def saveOrUpdateClient(client: Client): ClientRepository #> Try[Unit] =
    ReaderTFuture { clients => clients.save(client) }

  def deleteClient(id: String): ClientRepository #> Try[Unit] =
    ReaderTFuture { clients => clients.del(id) }
}

trait AuthService extends HttpService with CORSSupport {
  import AuthJsonProtocol._

  val authenticated: Directive1[OAuthCred] = authenticate(OAuth2Auth(validateToken, "auth"))

  val authorized: List[String] => Directive1[OAuthCred] = {
    import shapeless._
    (scope:List[String]) => authenticated.hflatMap {
      case c :: HNil =>  if (c.anyScope(scope)) provide(c) else reject
      case _ => reject
    }
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
    cors {
      path("token") {
        post {
          entity(as[AccessTokenRequest]) { //gran_type in ["password", ...]
            dto =>
              onSuccess(authorizeGrant(dto)(AppContext(new EsClientRepository, new EsUserRepository))) {
                r =>
                  r match {
                    case scala.util.Success(token) => complete(token)
                    case scala.util.Failure(ex) => complete(ex)
                  }
              }
          }
        }
      }
    }~
    cors {
      path("me") {
        get {
          authorized(List("auth")) {
            cred => complete(getUser(cred)(AppContext(new EsClientRepository, new EsUserRepository)))
          }
        }
      }
    }

  def authorizeGrant(data: AccessTokenRequest):AppContext #> Try[AccessToken] = {
    if (data.grantType != "password")
      throw new Exception ("Not supported")

    ReaderTFuture { ctx =>
      val a = (for {
        u <- optionT(Future(auth1(data.username.get, data.password.get)))
      } yield u).run

      for {
        x <- a
        u <- ctx.users.findByUsername(x.get.username);
        v = u match {
          case Some(y) => {
            Some(y)
          }
          case None => Some(User(java.util.UUID.randomUUID.toString, data.username.get, data.password.get)) //TODO: hash password
        }
        r <- ctx.users.save(v.get)
      } yield Try(buildAccessToken(v.get, data.scope, ""))
    }
  }

  def authorize(data: AuthorizeRequest): AppContext #> Try[(String, AccessToken)] = {
    ReaderTFuture { ctx =>
      val a = (for {
        //u <- optionT(ctx.users.findByUsername(data.username.get))
        u <- optionT(Future(auth1(data.username.get, data.password.get)))
      } yield u).run

      for {
        x <- a
        c <- Future.successful(x match {
          case Some(y) => {
            Some(User(java.util.UUID.randomUUID.toString, data.username.get, ""))
          }
          case None => None
        })
        r <- ctx.users.save(c.get)
      } yield Try((data.redirectUri.get, buildAccessToken(c.get, data.scope, data.state)))
    }
  }

  def getUser(cred: OAuthCred): AppContext #> Option[User] =
    ReaderTFuture {
      ctx => ctx.users.find(cred.id)
    }

  def buildAccessToken(u: User, scope:String, state:String) = {
    import java.util.Base64

    val token = new String(Base64.getEncoder.encode(s"${u.userId}:${scope}:${System.currentTimeMillis + 1800000}|${Utils.getHmac(u.userId + scope + (System.currentTimeMillis + 1800000))}".getBytes), "UTF-8")
    AccessToken(token, "token", scope, state)
  }

  def auth1(user: String, pass: String): Option[User] = {
    val (searchUser, searchPass) = ("cn=admin,dc=monolito,dc=com", "Admin123")
    ldapContext(searchUser, searchPass) match {
      case Right(searchContext) =>
        val result = auth2(searchContext, user, pass)
        searchContext.close()
        result
      case Left(ex) =>
        println("Could not authenticate with search user '%s': %s" format (searchUser, ex))
        None
    }
  }

  def auth2(searchContext: InitialLdapContext, user: String, pass: String): Option[User] = {
    query(searchContext, user) match {
      case entry :: Nil => auth3(entry, pass)
      case Nil =>
        //log.warning("User '{}' not found (search filter and search base ", user)
        None
      case entries =>
        //log.warning("Expected exactly one search result for search filter and search base , but got {}" , entries.size)
        None
    }
  }

  def auth3(entry: LdapQueryResult, pass: String): Option[User]= {
    println(s"auth3: $entry")
    ldapContext(entry.fullName, pass) match {
      case Right(authContext) =>

        authContext.close()
        //config.createUserObject(entry)
        Some(User(entry.name, "", ""))
      case Left(ex) =>
        //log.info("Could not authenticate user '{}': {}", entry.fullName, ex)
        None
    }
  }

  def ldapContext(user: String, pass: String): Either[Throwable, InitialLdapContext] = {
    scala.util.control.Exception.catching(classOf[NamingException]).either {
      val env = new Hashtable[AnyRef, AnyRef]
      env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
      env.put(Context.SECURITY_PRINCIPAL, user)
      env.put(Context.SECURITY_CREDENTIALS, pass)
      env.put(Context.SECURITY_AUTHENTICATION, "simple")

      val conf = List((javax.naming.Context.PROVIDER_URL, "ldap://devubu.monolito.com:389"))

      for ((key, value) <- conf) env.put(key, value)

      new InitialLdapContext(env, null)
    }
  }

  def query(ldapContext: InitialLdapContext, user: String): List[LdapQueryResult] = {
    println("querying %s" format user)
    val results: NamingEnumeration[SearchResult] = ldapContext.search(
      "OU=People,DC=monolito,DC=com",
      "(uid=%s)" format user,
      searchControls(user))
    results.asScala.toList.map(searchResult2LdapQueryResult)
  }

  def searchControls(user: String) = {
    val searchControls = new SearchControls
    searchControls.setSearchScope(SearchControls.SUBTREE_SCOPE)
    searchControls.setReturningAttributes(Array("givenName", "sn"))
    searchControls
  }

  def searchResult2LdapQueryResult(searchResult: SearchResult): LdapQueryResult = {
    import searchResult._
    LdapQueryResult(
      name = getName,
      fullName = getNameInNamespace,
      className = getClassName,
      relative = isRelative,
      obj = getObject,
      attrs = getAttributes.getAll.asScala.toSeq.map(a => a.getID -> attribute2LdapAttribute(a))(collection.breakOut))
  }

  def attribute2LdapAttribute(attr: Attribute): LdapAttribute = {
    LdapAttribute(
      id = attr.getID,
      ordered = attr.isOrdered,
      values = attr.getAll.asScala.toSeq.map(v => if (v != null) v.toString else ""))
  }
}
