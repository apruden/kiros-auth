package com.monolito.kiros.auth.data

import scala.concurrent.Future
import com.monolito.kiros.auth.model.User

/**
 * @author alex
 */
trait UserRepository {
  def get(id: String): Future[Option[User]]
}

class MongoUserRepository extends UserRepository {
  def get(id: String): Future[Option[User]] = ???
}

object MongoUserRepository {

}