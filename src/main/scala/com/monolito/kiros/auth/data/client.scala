package com.monolito.kiros.auth.data

import com.monolito.kiros.auth.model._
import scala.concurrent.Future
import reactivemongo.api._
import reactivemongo.bson._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import Scalaz._
import scalaz.OptionT._
import org.slf4j.LoggerFactory
import scala.util.Try


trait ClientRepository {
  def get(id: String): Future[Option[Client]]
  def save(client: Client): Future[Try[Unit]]
  def del(id: String): Future[Try[Unit]]
}

class MongoClientRepository() extends ClientRepository {
  import MongoClientRepository._

  val db = conn("auth")
  val collection = db("client")

  implicit object ClientTypeBSONWriter extends BSONDocumentWriter[ClientType] {
    def write(ct: ClientType): BSONDocument = BSONDocument("type" -> BSONString(ct.name))
  }

  implicit object ClientTypeBSONReader extends BSONDocumentReader[ClientType] {
    def read(doc: BSONDocument): ClientType = doc.getAs[String]("type").get match {
      case "public" => PUBLIC
      case "confidential" => CONFIDENTIAL
      case _ => throw new Exception("invalid type")
    }
  }

  implicit val reader: BSONDocumentReader[Client] = Macros.reader[Client]
  implicit val writer: BSONDocumentWriter[Client] = Macros.writer[Client]

  def get(name: String): Future[Option[Client]] =
    for {
      c <- collection.find(BSONDocument("name" -> name)).one[Client]
    } yield c

  def save(client: Client): Future[Try[Unit]] =
    for {
      r <- collection.update(BSONDocument("name" -> client.name), client, upsert = true)
      if r.ok
    } yield scala.util.Success(())

  def del(name: String): Future[Try[Unit]] =
    for {
      r <- collection.remove(BSONDocument("name" -> name))
      if r.ok
    } yield scala.util.Success(())
}

object MongoClientRepository {
  val logger = LoggerFactory.getLogger(this.getClass)

  lazy val driver = new MongoDriver
  lazy val conn = driver.connection(List("localhost"))
}