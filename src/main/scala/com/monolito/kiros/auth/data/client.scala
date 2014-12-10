package com.monolito.kiros.auth.data

import com.monolito.kiros.auth.model.Client
import scala.concurrent.Future
import scala.util.Try

trait ClientRepository {
  def get(id: String): Future[Option[String]]
  def save(client: Client): Client
  def del(id: String): Unit
}

class MongoClientRepository() extends ClientRepository {
  def get(id: String):Future[Option[String]] = ???
  def save(client: Client): Client= ???
  def del(id: String): Unit = ???
}