package com.monolito.kiros.auth.data

import com.monolito.kiros.auth._
import com.monolito.kiros.auth.model._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.source.DocumentMap
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.mappings.FieldType._
import org.elasticsearch.action.get.GetResponse
import scala.collection.JavaConversions._


trait Repository[T] {
  def find(tid: String): Future[Option[T]]
  def findAll(limit: Int, offset: Int, filters:Option[Map[String, Any]]=None): Future[List[T]]
  def save(t: T): Future[Try[Unit]]
  def del(id: String): Future[Try[Unit]]
}

trait ClientRepository extends Repository[Client]

trait UserRepository extends Repository[User]

trait EsRepository[T<:DocumentMap] extends Repository[T] {
  import EsRepository._

  val indexName: String
  val docType: String
  implicit val mapper: MapConvert[T]

  def find(tid: String): Future[Option[T]] =
    for {
      c <- client.execute { get id tid from indexName -> docType }
    } yield if (c.getSource != null) Some(c.getSource.toMap.convert[T]) else None

  def findAll(limit: Int, offset: Int, filters:Option[Map[String, Any]]=None): Future[List[T]] =
      for {
        r <- client.execute { search in indexName -> docType }
        c <- Future.successful { r.getHits.getHits }
        x <- Future.successful { c.map(y => y.getSource).toList }
      } yield x.map(z => z.toMap.convert[T])

  def save(t: T): Future[Try[Unit]] =
    for {
      c <- client.execute(index into indexName -> docType doc t)
    } yield scala.util.Success(())

  def del(tid: String): Future[Try[Unit]] =
    for {
      c <- client.execute(delete id tid from indexName -> docType)
    } yield scala.util.Success(())
}

object EsRepository {
  val client = ElasticClient.remote("localhost", 9300)

  def createIndex() = client.execute {
      create index "auth" mappings {
        "clients" source true as (
          "clientId" typed StringType index "not_analyzed",
          "name" typed StringType,
          "clientType" typed StringType index "not_analyzed",
          "redirectUri" typed StringType
        )
        "users" source true as (
          "userId" typed StringType index "not_analyzed",
          "username" typed StringType,
          "password" typed StringType index "not_analyzed"
        )
      } shards 4
    }.await

  if (!client.execute { status() }.await.getIndices().contains("auth")) createIndex()
}
