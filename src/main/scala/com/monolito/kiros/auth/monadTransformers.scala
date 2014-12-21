package com.monolito.kiros.auth

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

import scalaz._
import Scalaz._

/**
 * @author alex
 */
class monadTransformers {
  
  case class FutureT[F[_], A](run: F[A]) {
    def map[B](f: A => B)(implicit F: Functor[F]): FutureT[F, B] = FutureT(F.map(run)(x => f(x)))

    def flatMap[B](f: A => FutureT[F, B])(implicit F: Monad[F]): FutureT[F, B] = FutureT(F.bind(run)(x => f(x).run))
  }

  trait FutureTFunctor[F[_]] extends Functor[({ type λ[α] = FutureT[F, α] })#λ] {
    implicit def F: Functor[F]

    override def map[A, B](fa: FutureT[F, A])(f: A => B): FutureT[F, B] = fa map f
  }

  implicit def futureTFunctor[F[_], A](implicit F0: Functor[F]): Functor[({ type λ[α] = FutureT[F, α] })#λ] = new FutureTFunctor[F] {
    implicit def F: Functor[F] = F0
  }

  implicit val tryMonad = new Monad[Try] {
    def point[A](a: => A): Try[A] = Try { a }
    def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa flatMap f
  }

  type FutureTTry[A] = FutureT[Try, A]

  object FutureTTry {
    def apply[A](f: Try[A]): FutureTTry[A] = new FutureTTry(f)
  }

  type ReaderTFutureTTry[A, B] = ReaderT[FutureTTry, A, B]

  object ReaderTFutureTTry extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => FutureTTry[B]): ReaderTFutureTTry[A, B] = Kleisli(f)
    def pure[A, B](r: => FutureTTry[B]): ReaderTFutureTTry[A, B] = Kleisli(_ => r)
  }

  implicit val futureMonad = new Monad[Future] {
    def point[A](a: => A): Future[A] = Future { a }
    def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
  }
}