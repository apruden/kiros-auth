package com.monolito.kiros.auth.data

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scala.collection.JavaConversions._
import com.monolito.kiros.auth._
import com.monolito.kiros.auth.model._
import java.time.Instant

class EsClientRepository extends EsRepository[Client] with ClientRepository {
  val indexName = "auth"
  val docType = "clients"

  implicit val mapper: MapConvert[Client] = new MapConvert[Client] {
    def conv(values: Map[String, Any]): Client =
      Client(
        values.get("clientId").get.toString,
        values.get("name").get.toString,
        values.get("clientType").get.toString match {
          case "public" => PUBLIC
          case "confidential" => CONFIDENTIAL
        },
        values.get("redirectUri").get.toString)
  }
}

class EsUserRepository extends EsRepository[User] with UserRepository {
  import EsRepository._
  import com.monolito.kiros.commons.EsClient._

  val indexName = "auth"
  val docType = "users"

  implicit val mapper = new MapConvert[User] {
    def conv(values: Map[String, Any]): User = User(
      values.get("userId").get.toString,
      values.get("username").get.toString,
      values.get("password").get.toString)
  }

  def findByUsername(username: String): Future[Option[User]] =
    for {
      r <- query(indexName, docType, Map("query" -> Map("term" -> Map("username" -> username))))
    } yield r.map(_.convert[User]).headOption
}
