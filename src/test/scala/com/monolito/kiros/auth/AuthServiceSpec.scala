package com.monolito.kiros.auth

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._
import StatusCodes._

class AuthServiceSpec extends Specification with Specs2RouteTest with AuthService {
  def actorRefFactory = system

  "AuthService" should {

    "return a greeting for GET requests to the root path" in {
      Get() ~> authRoutes ~> check {
        responseAs[String] must contain("kiros-auth")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/non_existant/url") ~> authRoutes ~> check {
        handled must beFalse
      }
    }

    "return a MethodNotAllowed error for PUT requests to the root path" in {
      Put() ~> sealRoute(authRoutes) ~> check {
        status === MethodNotAllowed
        responseAs[String] === "HTTP method not allowed, supported methods: GET"
      }
    }

    "unmarshall authorize data" in {
      Post("/authorize", HttpEntity(MediaTypes.`application/x-www-form-urlencoded`,
        "scope=a&state=b&response_type=code&client_id=1&redirect_uri=a")) ~> authRoutes ~> check {
        responseAs[String] === ""
      }
    }

    "unmarshall client data" in {
      Post("/clients", HttpEntity(MediaTypes.`application/x-www-form-urlencoded`,
        "name=toto&client_type=public&redirect_uri=http%3A%2F%2Flocalhost")) ~> authRoutes ~> check {
        responseAs[String] === "OK"
      }
    }
  }
}
