package com.monolito.kiros.auth

import spray.httpx.unmarshalling._
import spray.util._
import spray.http._

case class AuthorizeDto(responseType: String, clientId: String, redirectUri: String,
  scope: String, state: String)

object AuthorizeDto {
  val args = List("response_type", "client_id", "redirect_uri", "scope", "state")

  implicit val AuthorizeDtoUnmarshaller =
    Unmarshaller.delegate[FormData, AuthorizeDto](MediaTypes.`application/x-www-form-urlencoded`) { data =>
      {
        (AuthorizeDto.apply _).tupled(args.map { a => data.fields.find(p => p._1 == a).get._2 } match {
          case List(a1, a2, a3, a4, a5) => (a1, a2, a3, a4, a5)
        })
      }
    }
}

case class TokenDto(username: String, password: String, grantType: String, scope: String,
  clientId: String, clientSecret: String, refreshToken: String)

object TokenDto {
  val args = List("username", "password", "grant_type", "scope", "client_id", "client_secret", "refresh_token");

  implicit val TokenDtoUnmarshaller =
    Unmarshaller.delegate[FormData, TokenDto](MediaTypes.`application/x-www-form-urlencoded`) { data =>
      {
        (TokenDto.apply _).tupled(args.map { a => data.fields.find(p => p._1 == a).get._2 } match {
          case List(a1, a2, a3, a4, a5, a6, a7) => (a1, a2, a3, a4, a5, a6, a7)
        })
      }
    }
}
