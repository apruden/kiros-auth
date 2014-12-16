package com.monolito.kiros.auth

import spray.httpx.unmarshalling._
import spray.util._
import spray.http._

case class AuthorizationCodeGrantResponse(code: String, state: String)

case class ImplicitGrantResponse(accessToken: String, tokenType: String, expiresIn: Int, scope: String, state: Option[String])

case class ErrorReponse(error: String, errorDescription: String, errorUri: String, state: String)

//TODO: Resources owner password credentials  https://tools.ietf.org/html/rfc6749 (4.3)

case class AuthorizeRequest(username:String, password:String, clientId: String, redirectUri: String, scope: String, state: String, responseType: String)

object AuthorizeRequest {
  val args = List("username", "password","response_type", "client_id", "redirect_uri", "scope", "state")

  implicit val authorizeRequestUnmarshaller =
    Unmarshaller.delegate[FormData, AuthorizeRequest](MediaTypes.`application/x-www-form-urlencoded`) { data =>
      (AuthorizeRequest.apply _).tupled( extractArgs (args, data) match {
        case List(a1, a2, a3, a4, a5, a6, a7) => (a1.get, a2.get, a3.get, a4.get, a5.get, a6.get, a7.get)
      })
    }
}

case class AccessTokenRequest(grantType: String, scope: String, 
    username: Option[String], password: Option[String], clientId: Option[String],
    clientSecret: Option[String], refreshToken: Option[String])

object AccessTokenRequest {
  val args = List("username", "password", "grant_type", "scope", "client_id", "client_secret", "refresh_token");

  implicit val accessTokenRequestUnmarshaller =
    Unmarshaller.delegate[FormData, AccessTokenRequest](MediaTypes.`application/x-www-form-urlencoded`) { data =>
    (AccessTokenRequest.apply _).tupled(extractArgs (args, data) match {
      case List(a1, a2, a3, a4, a5, a6, a7) => (a1.get, a2.get, a3, a4, a5, a6, a7)
    })
  }
}
