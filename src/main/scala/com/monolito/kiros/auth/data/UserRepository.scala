package com.monolito.kiros.auth.data

import scala.concurrent.Future
import com.monolito.kiros.auth.model.User
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import reactivemongo.api._
import reactivemongo.bson._

trait UserRepository {
  def get(id: String): Future[Option[User]]
  def save(user: User): Future[Try[Unit]]
  def del(id: String): Future[Try[Unit]]
}

class MongoUserRepository extends UserRepository {
  import MongoUserRepository._

  implicit val reader: BSONDocumentReader[User] = Macros.reader[User]
  implicit val writer: BSONDocumentWriter[User] = Macros.writer[User]
  
  val db = conn("auth")
  val collection = db("user")

  def get(id: String): Future[Option[User]] =
    for {
      c <- collection.find(BSONDocument("userId" -> id)).one[User]
    } yield c

  def save(user: User): Future[Try[Unit]] =
    for {
      r <- collection.update(BSONDocument("userId" -> user.userId), user, upsert = true)
      if r.ok
    } yield scala.util.Success(())

  def del(id: String): Future[Try[Unit]] = ???
}

object MongoUserRepository {
  lazy val driver = new MongoDriver
  lazy val conn = driver.connection(List("localhost"))
}