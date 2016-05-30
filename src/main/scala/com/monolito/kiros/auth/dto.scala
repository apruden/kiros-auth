package com.monolito.kiros.auth

import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.Multipart.FormData
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers

case class AuthorizationCodeGrantResponse(code: String, state: String)

case class ImplicitGrantResponse(accessToken: String, tokenType: String, expiresIn: Int, scope: String, state: Option[String])

case class ErrorReponse(error: String, errorDescription: String, errorUri: String, state: String)

//TODO: Resources owner password credentials  https://tools.ietf.org/html/rfc6749 (4.3)

case class AuthorizeRequest(formToken:Option[String], username: Option[String], password: Option[String], clientId: String, redirectUri: Option[String], scope: String, state: String, responseType: String)

object AuthorizeRequest {
  val args = List("form_token", "username", "password", "client_id", "redirect_uri", "scope", "state", "response_type")
  
  implicit val authorizeRequestUnmarshaller = PredefinedFromEntityUnmarshallers.defaultUrlEncodedFormDataUnmarshaller.map { data =>
    (AuthorizeRequest.apply _).tupled(extractArgs(args, data.fields.toMap) match {
      case List(a0, a1, a2, a3, a4, a5, a6, a7) => {
        (a0, a1, a2, a3.get, a4, a5.get, a6.get, a7.get)
      }
    })
  }
}

case class AccessTokenRequest(grantType: String, scope: String,
  username: Option[String], password: Option[String], clientId: Option[String],
  clientSecret: Option[String], refreshToken: Option[String])

object AccessTokenRequest {
  val args = List("grant_type", "scope", "username", "password", "client_id", "client_secret", "refresh_token");

  implicit val accessTokenRequestUnmarshaller = PredefinedFromEntityUnmarshallers.defaultUrlEncodedFormDataUnmarshaller.map { data =>
      (AccessTokenRequest.apply _).tupled(extractArgs(args, data.fields.toMap) match {
        case List(a1, a2, a3, a4, a5, a6, a7) => (a1.get, a2.get, a3, a4, a5, a6, a7)
      })
  }
}
