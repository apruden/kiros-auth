package com.monolito.kiros

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import Scalaz._
import scala.util.Try
import com.typesafe.config._
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext
import akka.http.scaladsl.model.Multipart.FormData


package object auth {
  val conf = ConfigFactory.load()
  val logger = LoggerFactory.getLogger("auth")

  def getLdapCredentials() = (conf.getString("kiros.ldap-user"), conf.getString("kiros.ldap-pass"))
  
  def extractArgs(args: List[String], data: Map[String, String]): List[Option[String]] =
    args.map(a => data.find(p => p._1 == a)).map(b =>
      b match {
        case Some(x) => Some(x._2)
        case None => None
      })

  type #>[E, A] = Kleisli[Future, E, A]

  object ReaderTFuture extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Future[B]): A #> B = Kleisli(f)
    def pure[A, B](r: => Future[B]): A #> B = Kleisli(_ => r)
  }

  implicit def toReader[C, R](f: C => R) = Reader(f)
  implicit def toReaderFutureT[C, R](f: Reader[C, Future[R]]) = ReaderTFuture(f)

  trait MapConvert[A] {
    def conv(values: Map[String, Any]): A
  }

  implicit class Map2Class(values: Map[String, Any]){
    def convert[A](implicit mapper: MapConvert[A]) = mapper conv (values)
  }
}
