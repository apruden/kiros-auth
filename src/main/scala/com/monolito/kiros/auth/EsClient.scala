package com.monolito.kiros.auth

import akka.actor.ActorSystem
import akka.io.IO
import spray.http._
import spray.client.pipelining._
import spray.httpx.encoding.{Gzip, Deflate}
import spray.json._
import spray.json.DefaultJsonProtocol._
import spray.json.{JsonFormat, DefaultJsonProtocol}
import spray.httpx.SprayJsonSupport._
import scala.concurrent.Future
import scala.util._

object EsJsonProtocol extends DefaultJsonProtocol {
  implicit object AnyJsonFormat extends JsonFormat[Any] {
    def write(x: Any) = x match {
      case n: Int => JsNumber(n)
      case s: String => JsString(s)
      case b: Boolean => if (b ) JsTrue else JsFalse
      case q: Seq[Any] => JsArray(q.map(write(_)).toVector)
      case o: Map[String, Any] => JsObject(o.map(e => (e._1, write(e._2))))
      case x => JsString(x.toString)
    }

    def read(value: JsValue): Any = value match {
      case JsNumber(n) => n.intValue()
      case JsString(s) => s
      case JsTrue => true
      case JsFalse => false
      case x: JsArray => x.elements.map(read(_))
      case JsNull => null
      case o: JsObject => o.fields.map(e => (e._1, read(e._2)))
    }
  }
}

object EsClient {
  import com.monolito.kiros.auth.conf
  import concurrent.ExecutionContext.Implicits._
  import SprayJsonSupport._
  import EsJsonProtocol._

  implicit val system = ActorSystem()

  val host = conf.getString("kiros.auth.index-url")

  val pipeline: HttpRequest => Future[Map[String, Any]] = sendReceive ~> unmarshal[Map[String,Any]]

  def createIndex (mapping: Map[String, Any]): Future[Unit] =
    for {
      a <- pipeline { Post(host, mapping)}
      r <- Future {()}
    } yield r

  def put (typ: String, id: String, document: Map[String, Any]): Future[Unit] =
    for {
      a <- pipeline { Put(s"$host/$typ/$id", document) }
      r <- Future {()}
    } yield r

  def get (typ: String, id: String): Future[Option[Map[String, Any]]] =
    for {
      d <- pipeline { Get(s"$host/$typ/$id") }
      r <- Future { d.get("_source").asInstanceOf[Option[Map[String, Any]]] }
    } yield r

  def del (typ: String, id: String): Future[Unit] = ???

  def query (typ: String, query: Map[String, Any]): Future[List[Map[String, Any]]] =
    for {
      d <- pipeline { Post(s"$host/$typ/_search", query) }
      r <- Future {
        d.getOrElse("hits", Map()).asInstanceOf[Map[String, Any]].getOrElse("hits", List()).asInstanceOf[Seq[Map[String, Any]]].map(_.get("_source").get.asInstanceOf[Map[String, Any]]).toList
      }
    } yield r
}
