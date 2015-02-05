package com.monolito.kiros.auth.model

import java.net.URL
import com.sksamuel.elastic4s.source.DocumentMap

sealed trait ClientType { def name: String }

case object PUBLIC extends ClientType { val name = "public" }
case object CONFIDENTIAL extends ClientType { val name = "confidential" }

case class Client(clientId:String, name: String, clientType: ClientType, redirectUri: String) extends DocumentMap with Entity {
  require(new URL(redirectUri).getProtocol == "https")

  override def map = Map[String,Any] (
    "clientId" -> clientId,
    "name" -> name,
    "clientType" -> clientType.name,
    "redirectUri" -> redirectUri
    )

  def getId = clientId
}
