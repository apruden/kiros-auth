package com.monolito.kiros

import spray.httpx.unmarshalling._
import spray.util._
import spray.http._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz.Monad

package object auth {

  def extractArgs(args: List[String], data: FormData): List[Option[String]] =
    args.map(a => data.fields.find(p => p._2 == a)).map(b =>
      b match {
        case Some(x) => Some(x._2)
        case None => None
      })

  implicit val futureMonad = new Monad[Future] {
    def point[A](a: => A): Future[A] =
      Future { a }
    def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  }

}