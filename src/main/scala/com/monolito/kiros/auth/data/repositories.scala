package com.monolito.kiros.auth.data

import com.monolito.kiros.auth._
import com.monolito.kiros.auth.model._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.collection.JavaConversions._
import java.time.Instant

trait Repository[T] {
  def find(tid: String): Future[Option[T]]
  def findAll(offset:Int, limit: Int, filters:Option[String]=None): Future[List[T]]
  def save(t: T): Future[Try[Unit]]
  def del(id: String): Future[Try[Unit]]
}

trait ClientRepository extends Repository[Client]

trait UserRepository extends Repository[User] {
  def findByUsername(username: String): Future[Option[User]]
}

trait EsRepository[T<:Entity] extends Repository[T] {
  import EsRepository._
  import EsClient._

  val indexName: String
  val docType: String
  implicit val mapper: MapConvert[T]

  def find(tid: String): Future[Option[T]] =
    for {
      c <- get (docType, tid)
    } yield if (c.isDefined) Some(c.get.convert[T]) else None

  def findAll(offset: Int, limit: Int, filters:Option[String]=None): Future[List[T]] =
      for {
        r <- query (docType, Map("query"-> Map("match_all" -> Map())) )
      } yield r.map(_.convert[T])

  def save(t: T): Future[Try[Unit]] =
    for {
      c <- put(docType, t.getId, t.map)
    } yield scala.util.Success(())

  def del(tid: String): Future[Try[Unit]] = ???

  def getId(t: T) = t.map.get("id").get
}

object EsRepository {
  import EsClient._

  def tryCreateIndex() = {
    println("creating index ....")

    createIndex(Map("settings" -> Map("number_of_shards" -> 1, "number_of_replicas" -> 1),
      "mappings" -> Map(
        "clients" -> Map(
          "properties" -> Map(
            "clientId" -> Map("type" -> "string", "index"-> "not_analyzed"),
            "name" -> Map("type" -> "string"),
            "clientType" -> Map("type" -> "string", "index"-> "not_analyzed"),
            "redirectUrl" -> Map("type" -> "string", "index"-> "not_analyzed")
          )
        ),
      "users" -> Map(
        "properties" -> Map(
          "userId" -> Map("type" -> "string", "index"-> "not_analyzed"),
          "username" -> Map("type" -> "string", "index"-> "not_analyzed", "fields" -> Map ("an" -> Map("type" -> "string") )),
          "password" -> Map("type" -> "string", "index"-> "not_analyzed")
        )
      )
    )
  ))

    ()
  }
}
