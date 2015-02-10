package com.monolito.kiros.auth

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import spray.http._
import spray.routing._
import spray.http.MediaTypes._
import spray.http.HttpHeaders._
import spray.http.HttpMethods._
import spray.routing.authentication._


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


