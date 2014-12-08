package com.monolito.kiros.auth.data

import reactivemongo.api._
import reactivemongo.bson._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class RefreshTokenRepository {

  val driver = new MongoDriver
  val connection = driver.connection(List("localhost"))
  val db = connection("plugin")
  val collection = db("articles")

}