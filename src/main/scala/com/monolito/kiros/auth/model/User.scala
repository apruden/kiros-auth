package com.monolito.kiros.auth.model
import com.sksamuel.elastic4s.source.DocumentMap


case class User(userId: String, username: String, password: String) extends DocumentMap with Entity {
  override def map = Map[String, Any](
    "userId" -> userId,
    "username" -> username,
    "password" -> password
  )

  def getId = userId
}
