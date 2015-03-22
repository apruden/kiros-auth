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
import javax.naming.directory.{ SearchControls, SearchResult, Attribute, DirContext,  ModificationItem, BasicAttribute }
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
      } ~
      path("me") {
        get {
          authorized(List("auth")) {
            cred => complete(getUser(cred)(AppContext(new EsClientRepository, new EsUserRepository)))
          }
        } ~
        post {
          authorized(List("auth")) { cred =>
            formFields('oldPassword, 'newPassword) ( (oldPassword, newPassword) =>
                  onSuccess(changePassword(cred, oldPassword, newPassword)(AppContext(new EsClientRepository, new EsUserRepository))) {
                    r =>
                      r match {
                        case scala.util.Success(res) => complete(res)
                        case scala.util.Failure(ex) => complete(ex)
                      }
                  }
                )
          }
        }
      }
    }

  def changePassword(cred: OAuthCred, oldPassword: String, newPassword: String):AppContext #> Try[String] =
    ReaderTFuture { ctx =>
      for {
        u <- ctx.users.find(cred.id)
        r <- Future { changePasswordInternal(u.get.username, oldPassword, newPassword).get }
      } yield Try(r)
    }

  def changePasswordInternal(username: String, oldPassword:String, newPassword: String): Option[String]= {
    val Array(un, dn) = username.split("@")
    val userDn = s"uid=$un,ou=People," + dn.split('.').map { "dc=" + _ }.mkString(",")
    ldapContext(userDn, oldPassword) match {
      case Right(authContext) =>
        val mods = Array(
            new ModificationItem(DirContext.REPLACE_ATTRIBUTE,
              new BasicAttribute("userPassword", newPassword)
            )
          )
        authContext.modifyAttributes(userDn, mods)
        authContext.close()
        Some("OK")
      case Left(ex) =>
        println(s">>>$ex")
        None
    }
  }

  def authorizeGrant(data: AccessTokenRequest):AppContext #> Try[AccessToken] = {
    if (data.grantType != "password")
      throw new Exception ("Not supported")

    val Array(username, domain) = data.username.get.split("@")

    ReaderTFuture { ctx =>
      val a = (for {
        u <- optionT(Future(auth1(username, data.password.get, domain.split('.'): _*)))
      } yield u).run

      for {
        x <- a
        u <- ctx.users.findByUsername(x.get.username);
        v = u match {
          case Some(y) => {
            Some(y)
          }
          case None => Some(User(java.util.UUID.randomUUID.toString, data.username.get, "")) //TODO: hash password
        }
        r <- ctx.users.save(v.get)
      } yield Try(buildAccessToken(v.get, data.scope, ""))
    }
  }

  def authorize(data: AuthorizeRequest): AppContext #> Try[(String, AccessToken)] = {
    val Array(username, domain) = data.username.get.split("@")

    ReaderTFuture { ctx =>
      val a = (for {
        u <- optionT(Future(auth1(username, data.password.get, domain.split('.'): _*)))
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

  def auth1(user: String, pass: String, dc: String*): Option[User] = {
    import com.monolito.kiros.auth.getLdapCredentials

    var (adminUser, adminPass) = getLdapCredentials()
    val (searchUser, searchPass) = (s"cn=$adminUser," + dc.map{ "dc=" + _ }.mkString(","), adminPass)
    ldapContext(searchUser, searchPass) match {
      case Right(searchContext) =>
        val result = auth2(searchContext, user, pass)
        searchContext.close()
        result
      case Left(ex) =>
        None
    }
  }

  def auth2(searchContext: InitialLdapContext, user: String, pass: String): Option[User] = {
    query(searchContext, user) match { // search user
      case entry :: Nil => auth3(entry, pass) //validate provided password
      case Nil =>
        None
      case entries =>
        None
    }
  }

  def auth3(entry: LdapQueryResult, pass: String): Option[User]= {
    ldapContext(entry.fullName, pass) match {
      case Right(authContext) =>
        authContext.close() //use valid
        Some(User(entry.name, "", ""))
      case Left(ex) =>
        None
    }
  }

  def ldapContext(user: String, pass: String): Either[Throwable, InitialLdapContext] = {
    import com.monolito.kiros.auth.conf

    scala.util.control.Exception.catching(classOf[NamingException]).either {
      val env = new Hashtable[AnyRef, AnyRef]
      env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
      env.put(Context.SECURITY_PRINCIPAL, user)
      env.put(Context.SECURITY_CREDENTIALS, pass)
      env.put(Context.SECURITY_AUTHENTICATION, "simple")

      val confTemp = List((javax.naming.Context.PROVIDER_URL, conf.getString("kiros.ldap-host")))

      for ((key, value) <- confTemp) env.put(key, value)

      new InitialLdapContext(env, null)
    }
  }

  def query(ldapContext: InitialLdapContext, user: String): List[LdapQueryResult] = {
    import com.monolito.kiros.auth.conf

    val (ou, Array(domain, ext)) = (conf.getString("kiros.ldap-ou"), conf.getString("kiros.ldap-domain").split('.'))
    ldapContext.search(
      s"OU=$ou,DC=$domain,DC=$ext",
      s"(uid=$user)",
      searchControls(user))
        .asScala
        .toList
        .map(searchResult2LdapQueryResult)
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
