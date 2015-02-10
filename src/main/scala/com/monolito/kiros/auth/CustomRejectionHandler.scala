package com.monolito.kiros.auth

import spray.http._
import spray.http.StatusCodes._
import spray.http.HttpHeaders._
import spray.routing._
import spray.routing.directives.RouteDirectives._
import spray.routing.AuthenticationFailedRejection._


object CustomRejectionHandler {
  import CORSSupport._

  implicit val corsDefault = RejectionHandler {
    case Nil => complete(NotFound, List(allowOriginHeader), "The requested resource could not be found.")

    case AuthenticationFailedRejection(cause, challengeHeaders) :: _ =>
      val rejectionMessage = cause match {
        case CredentialsMissing  => "Ups! The resource requires authentication, which was not supplied with the request"
        case CredentialsRejected => "Ups! The supplied authentication is invalid"
      }
      { ctx => ctx.complete(Unauthorized, allowOriginHeader :: challengeHeaders, rejectionMessage) }

    case AuthorizationFailedRejection :: _ =>
      complete(Forbidden, List(allowOriginHeader), "Ups! The supplied authentication is not authorized to access this resource")

    case CorruptRequestEncodingRejection(msg) :: _ =>
      complete(BadRequest, List(allowOriginHeader), "The requests encoding is corrupt:\n" + msg)

    case MalformedFormFieldRejection(name, msg, _) :: _ =>
      complete(BadRequest, List(allowOriginHeader), "The form field '" + name + "' was malformed:\n" + msg)

    case MalformedHeaderRejection(headerName, msg, _) :: _ =>
      complete(BadRequest, List(allowOriginHeader), s"The value of HTTP header '$headerName' was malformed:\n" + msg)

    case MalformedQueryParamRejection(name, msg, _) :: _ =>
      complete(BadRequest, List(allowOriginHeader), "The query parameter '" + name + "' was malformed:\n" + msg)

    case MalformedRequestContentRejection(msg, _) :: _ =>
      complete(BadRequest, List(allowOriginHeader), "The request content was malformed:\n" + msg)

    case rejections @ (MethodRejection(_) :: _) =>
      val methods = rejections.collect { case MethodRejection(method) => method }
      complete(MethodNotAllowed, List(Allow(methods: _*)), "HTTP method not allowed, supported methods: " + methods.mkString(", "))

    case rejections @ (SchemeRejection(_) :: _) =>
      val schemes = rejections.collect { case SchemeRejection(scheme) => scheme }
      complete(BadRequest, List(allowOriginHeader), "Uri scheme not allowed, supported schemes: " + schemes.mkString(", "))

    case MissingCookieRejection(cookieName) :: _ =>
      complete(BadRequest, List(allowOriginHeader), "Request is missing required cookie '" + cookieName + '\'')

    case MissingFormFieldRejection(fieldName) :: _ =>
      complete(BadRequest, List(allowOriginHeader), "Request is missing required form field '" + fieldName + '\'')

    case MissingHeaderRejection(headerName) :: _ =>
      complete(BadRequest, List(allowOriginHeader), "Request is missing required HTTP header '" + headerName + '\'')

    case MissingQueryParamRejection(paramName) :: _ =>
      complete(NotFound, List(allowOriginHeader), "Request is missing required query parameter '" + paramName + '\'')

    case RequestEntityExpectedRejection :: _ =>
      complete(BadRequest, List(allowOriginHeader), "Request entity expected but not supplied")

    case TooManyRangesRejection(_) :: _ =>
      complete(RequestedRangeNotSatisfiable, List(allowOriginHeader), "Request contains too many ranges.")

    case UnsatisfiableRangeRejection(unsatisfiableRanges, actualEntityLength) :: _ =>
      complete(RequestedRangeNotSatisfiable, allowOriginHeader :: List(`Content-Range`(ContentRange.Unsatisfiable(actualEntityLength))),
        unsatisfiableRanges.mkString("None of the following requested Ranges were satisfiable:\n", "\n", ""))

    case rejections @ (UnacceptedResponseContentTypeRejection(_) :: _) =>
      val supported = rejections.flatMap {
        case UnacceptedResponseContentTypeRejection(supported) => supported
        case _ => Nil
      }
      complete(NotAcceptable, List(allowOriginHeader), "Resource representation is only available with these Content-Types:\n" + supported.map(_.value).mkString("\n"))

    case rejections @ (UnacceptedResponseEncodingRejection(_) :: _) =>
      val supported = rejections.collect { case UnacceptedResponseEncodingRejection(supported) => supported }
      complete(NotAcceptable, List(allowOriginHeader), "Resource representation is only available with these Content-Encodings:\n" + supported.map(_.value).mkString("\n"))

    case rejections @ (UnsupportedRequestContentTypeRejection(_) :: _) =>
      val supported = rejections.collect { case UnsupportedRequestContentTypeRejection(supported) => supported }
      complete(UnsupportedMediaType, List(allowOriginHeader), "There was a problem with the requests Content-Type:\n" + supported.mkString(" or "))

    case rejections @ (UnsupportedRequestEncodingRejection(_) :: _) =>
      val supported = rejections.collect { case UnsupportedRequestEncodingRejection(supported) => supported }
      complete(BadRequest, List(allowOriginHeader), "The requests Content-Encoding must be one the following:\n" + supported.map(_.value).mkString("\n"))

    case ValidationRejection(msg, _) :: _ =>
      complete(BadRequest, List(allowOriginHeader), msg)
  }
}
