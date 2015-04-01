package com.monolito.kiros.auth.model

case class User(userId: String, username: String, password: String) extends Entity {
  override def map = Map[String, Any](
    "userId" -> userId,
    "username" -> username,
    "password" -> password
  )

  def getId = userId
}
