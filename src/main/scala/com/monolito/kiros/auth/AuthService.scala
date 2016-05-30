package com.monolito.kiros.auth
import java.io._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent._
import scala.util.Try

import com.monolito.kiros.auth.data.UserRepository
import com.monolito.kiros.auth.data.EsUserRepository
import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.EsClientRepository
import com.monolito.kiros.auth.model._
import scalaz.OptionT._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directive1
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.FileIO

import spray.json._
import scalaz._
import Scalaz.{get => _, put => _, _}

import com.monolito.kiros.commons.CorsSupport
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model._

import akka.http.scaladsl.server.Directive
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.unmarshalling._
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.MediaTypes._
import akka.stream.scaladsl.FileIO
import akka.stream.scaladsl.StreamConverters
import java.util.concurrent.TimeUnit
import com.monolito.kiros.commons.OAuthCred
import com.monolito.kiros.commons.OAuth2Support

import java.util.Hashtable
import javax.naming.{ Context, NamingException, NamingEnumeration }
import javax.naming.ldap.InitialLdapContext
import javax.naming.directory.{ SearchControls, SearchResult, Attribute, DirContext,  ModificationItem, BasicAttribute }
import scala.collection.JavaConverters._

import com.fasterxml.uuid.Generators
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers

case class LdapAttribute(
    id: String,
    ordered: Boolean,
    values: Seq[String]) {
  def value = if (values.isEmpty) "" else values.head
}

case class LdapQueryResult(
  name: String,
  fullName: String,
  className: String,
  relative: Boolean,
  obj: AnyRef,
  attrs: Map[String, LdapAttribute])

object AuthJsonProtocol {
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
}

trait AuthService extends CorsSupport with SprayJsonSupport with OAuth2Support {
  import spray.json.DefaultJsonProtocol._
  import AuthJsonProtocol._
  implicit val timeout = Timeout(5.seconds)
  implicit val system = ActorSystem("on-spray-can")
  implicit val executionContext = system.dispatcher
  implicit val materializer = ActorMaterializer()
  implicit val clientFormat = jsonFormat4(Client)
  implicit val accessTokenFormat = jsonFormat4(AccessToken)

  val generator = Generators.timeBasedGenerator()

  implicit val rawIntFromEntityUnmarshaller: FromEntityUnmarshaller[ClientType] =
    PredefinedFromEntityUnmarshallers.stringUnmarshaller.map(x => x match {
      case "public" => PUBLIC
      case "confidential" => CONFIDENTIAL
      case _ => throw new Exception("Invalid client type")
    })

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

  val authRoutes =
    pathSingleSlash {
      get {
        complete(HttpResponse(entity = HttpEntity(ContentTypes.`text/html(UTF-8)`,  html.index().toString)))
      }
    } ~
    path("authorize") { //authorize third-party application. should display an authorize form (web-application)
      get {
          parameters('client_id, 'scope, 'state, 'redirect_uri, 'response_type) {
            (client_id, scope, state, redirect_uri, response_type) => complete(
                HttpResponse(entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, html.grant_form(client_id, scope, state, redirect_uri).toString))    
            )
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
          authenticateOAuth2Async("", authenticator("auth")) {
            cred => complete(getUser(cred)(AppContext(new EsClientRepository, new EsUserRepository)))
          }
        } ~
        post {
          authenticateOAuth2Async("", authenticator("auth")) { cred =>
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
    val userDn = s"uid=$un,ou=" + conf.getString("kiros.ldap-ou") +"," + dn.split('.').map { "dc=" + _ }.mkString(",")
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
        None
    }
  }

  def authorizeGrant(data: AccessTokenRequest):AppContext #> Try[AccessToken] = {
    if (data.grantType != "password")
      throw new Exception ("Not supported")

    val Array(username, domain) = data.username.get.split("@")

    ReaderTFuture { ctx =>
      val a = (for {
        u <- optionT(Future { auth1(username, data.password.get, domain.split('.'): _*) })
      } yield u).run

      for {
        x <- a
        u <- ctx.users.findByUsername(data.username.get)
        v <- Future.successful {
          x.get
          u match {
            case Some(y) => {
              Some(y)
            }
            case None => Some(User(generator.generate().toString, data.username.get, "")) //TODO: hash password
          }
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
        c <- {
          Future.successful(x match {
          case Some(y) => {
            Some(User(generator.generate().toString, data.username.get, ""))
          }
          case None => None
        })
        }
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
