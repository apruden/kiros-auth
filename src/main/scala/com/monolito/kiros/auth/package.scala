package com.monolito.kiros

import spray.httpx.unmarshalling._
import spray.util._
import spray.http._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import Scalaz._
import scala.util.Try
import spray.routing.authentication._
import spray.http.HttpHeaders._
import shapeless._
import com.typesafe.config._

package object auth {
  val conf = ConfigFactory.load()

  def getLdapCredentials() = (conf.getString("kiros.ldap-user"), conf.getString("kiros.ldap-pass"))

  type OAuth2Authenticator[T] = Option[String] => Future[Option[T]]

  def extractArgs(args: List[String], data: FormData): List[Option[String]] =
    args.map(a => data.fields.find(p => p._1 == a)).map(b =>
      b match {
        case Some(x) => Some(x._2)
        case None => None
      })

  type #>[E, A] = Kleisli[Future, E, A]

  object ReaderTFuture extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Future[B]): A #> B = Kleisli(f)
    def pure[A, B](r: => Future[B]): A #> B = Kleisli(_ => r)
  }

  trait MapConvert[A] {
    def conv(values: Map[String, Any]): A
  }

  implicit class Map2Class(values: Map[String, Any]){
    def convert[A](implicit mapper: MapConvert[A]) = mapper conv (values)
  }

  implicit def toReader[C, R](f: C => R) = Reader(f)
  implicit def toReaderFutureT[C, R](f: Reader[C, Future[R]]) = ReaderTFuture(f)

  def validateToken(tok: Option[String]): Future[Option[OAuthCred]] = {
    import java.util.Base64
    val cred = if (tok.nonEmpty) {
      new String(Base64.getDecoder.decode(tok.get), "UTF-8").split('|').toList match {
        case List(data, hmac) =>
          data.split(':').toList match {
            case List(uid, scopes, expire) => {
              if(true)
                Some(OAuthCred(uid, scopes.split(' ').toList, expire.toLong))
              else
                None
            }
            case _ => None
          }
            case _ => None
      }
    } else None

    Future.successful { cred }
  }
}
