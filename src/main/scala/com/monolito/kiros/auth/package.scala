package com.monolito.kiros

import spray.httpx.unmarshalling._
import spray.util._
import spray.http._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import Scalaz._
import scala.util.Try

package object auth {

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

  implicit def toReader[C, R](f: C => R) = Reader(f)
  implicit def toReaderFutureT[C, R](f: Reader[C, Future[R]]) = ReaderTFuture(f)
}
