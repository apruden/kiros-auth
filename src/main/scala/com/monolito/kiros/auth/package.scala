package com.monolito.kiros

import spray.httpx.unmarshalling._
import spray.util._
import spray.http._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import Scalaz._

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

  type #>[E, A] = Kleisli[Future, E, A]

  object ReaderFutureT extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Future[B]): A #> B = Kleisli(f)
    def pure[A, B](r: => Future[B]): A #> B = Kleisli(_ => r)
  }

  object ReaderOptionT extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Option[B]): A =?> B = kleisli(f)
    def pure[A, B](r: => Option[B]): A =?> B = kleisli(_ => r)
  }

  implicit def toReader[C, R](f: C => R) = Reader(f)
  implicit def toReaderOptionT[C, R](f: Reader[C, Option[R]]) = ReaderOptionT(f)
  implicit def toReaderFutureT[C, R](f: Reader[C, Future[R]]) = ReaderFutureT(f)
}