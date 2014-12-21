package com.monolito.kiros.auth

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._
import StatusCodes._

class AuthServiceSpec extends Specification with Specs2RouteTest with AuthService with ClientService {
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

    "return authenticate form for implicit grant" in {
      Get("/authorize?scope=a&state=b&response_type=token&client_id=1&redirect_uri=https%3A%2F%2Flocalhost") ~> authRoutes ~> check {
        responseAs[String] must contain("form")
      }
    }

    "authenticate successfully for implicit grant" in {
      Post("/authorize", HttpEntity(MediaTypes.`application/x-www-form-urlencoded`, 
          "form_token=123&scope=a&state=b&client_id=1&response_type=token&username=toto&password=test&redirect_uri=https%3A%2F%2Flocalhost")) ~> authRoutes ~> check {
        responseAs[String] must contain("access_token")
      }
    }

    "unmarshall client data" in {
      Post("/clients", HttpEntity(MediaTypes.`application/x-www-form-urlencoded`,
        "name=toto&client_type=public&redirect_uri=https%3A%2F%2Flocalhost")) ~> clientRoutes ~> check {
        responseAs[String] === "OK"
      }
    }
  }
}
