package io.github.hamsters

import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}

@implicitNotFound("""Cannot create monad instance for type ${Box}.
If you are combining Future with another monad you might pass
an (implicit ec: ExecutionContext) parameter to your method
or import scala.concurrent.ExecutionContext.Implicits.global""")
trait Monad[Box[_]] extends Functor[Box] {

  def pure[A](a: A): Box[A]

  def flatMap[A, B](boxA: Box[A])(f: A => Box[B]): Box[B]

}


object Monad {

  implicit val optionMonad = new Monad[Option] {

    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](boxA: Option[A])(f: A => Option[B]) = boxA.flatMap(f)

    override def map[A, B](boxA: Option[A])(f: A => B) = boxA.map(f)
  }

  implicit val listMonad = new Monad[List] {

    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](boxA: List[A])(f: A => List[B]): List[B] = boxA.flatMap(f)

    override def map[A, B](boxA: List[A])(f: A => B): List[B] = boxA.map(f)
  }

  implicit def futureMonad(implicit ec: ExecutionContext) = new Monad[Future] {

    override def pure[A](x: A): Future[A] = Future.successful(x)

    override def flatMap[A, B](boxA: Future[A])(f: A => Future[B]) = {
      boxA.flatMap(f)
    }

    override def map[A, B](boxA: Future[A])(f: A => B) = {
      boxA.map(f)
    }
  }
}

