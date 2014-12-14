package com.monolito.kiros.auth.model

import java.net.URL

sealed trait ClientType { def name: String }

case object PUBLIC extends ClientType { val name = "public" }
case object CONFIDENTIAL extends ClientType { val name = "confidential" }

case class Client(name: String, clientType: ClientType, redirectUri: String) {
  require(new URL(redirectUri).getProtocol == "http")
}