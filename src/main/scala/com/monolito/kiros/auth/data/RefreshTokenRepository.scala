package com.monolito.kiros.auth.data

import reactivemongo.api._
import reactivemongo.bson._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import com.monolito.kiros.auth.model.RefreshToken


class MongoRefreshTokenRepository {
  lazy val driver = new MongoDriver
  lazy val conn = driver.connection(List("localhost"))

  val db = conn("auth")
  val collection = db("resfreshToken")

  def get(id: String): RefreshToken = ???
}
