package com.monolito.kiros.auth.data

import com.monolito.kiros.auth.model.Client

trait ClientRepository {
  def get(id: String): Client
  def save(client: Client): Unit
  def del(id: String): Unit
}

class MongoClientRepository() extends ClientRepository {
  def get(id: String): Client = ???
  def save(client: Client): Unit = ???
  def del(id: String): Unit = ???
}