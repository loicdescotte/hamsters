package io.github.hamsters

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}
import com.twitter.{util => twitter}

case class FutureOption[+A](future: Future[Option[A]]) extends AnyVal {
  def flatMap[B](f: A => FutureOption[B])(implicit ec: ExecutionContext): FutureOption[B] = {
    val newFuture = future.flatMap {
      case Some(a) => f(a).future
      case None => Future.successful(None)
    }
    FutureOption(newFuture)
  }

  def map[B](f: A => B)(implicit ec: ExecutionContext): FutureOption[B] = {
    FutureOption(future.map(option => option map f))
  }

  def filter(p: (A) ⇒ Boolean)(implicit ec: ExecutionContext): FutureOption[A] = filterWith(p)

  def filterWith(p: (A) ⇒ Boolean)(implicit ec: ExecutionContext): FutureOption[A] = {
    FutureOption(future.map {
      case Some(a) => if (p(a)) Some(a) else None
      case _ => None
    })
  }
}


case class FutureEither[L, +R](future: Future[Either[L, R]]) extends AnyVal {
  def flatMap[R2](f: R => FutureEither[L, R2])(implicit ec: ExecutionContext): FutureEither[L, R2] = {
    val newFuture = future.flatMap {
      case Right(r) => f(r).future
      case Left(l) => Future.successful(Left(l))
    }
    FutureEither(newFuture)
  }

  def map[R2](f: R => R2)(implicit ec: ExecutionContext): FutureEither[L, R2] = {
    FutureEither(future.map(either => either.right map f))
  }

  def filter(p: (R) ⇒ Boolean)(implicit ec: ExecutionContext): FutureEither[String, R] = filterWith(p)

  def filterWith(p: (R) ⇒ Boolean)(implicit ec: ExecutionContext): FutureEither[String, R] = {
    FutureEither(future.map {
      case Right(r) => if (p(r)) Right(r) else Left("No value matching predicate")
      case _ => Left("No value matching predicate")
    })
  }

  def filterWithDefault(p: (R) ⇒ Boolean, default: L)(implicit ec: ExecutionContext): FutureEither[L, R] = {
    FutureEither(future.map {
      case Right(r) => if (p(r)) Right(r) else Left(default)
      case _ => Left(default)
    })
  }
}

import TwitterFutureOps._
case class TwitterFutureOption[+A](future: twitter.Future[Option[A]]) extends AnyVal {
  def flatMap[B](f: A => TwitterFutureOption[B])(implicit ec: ExecutionContext): TwitterFutureOption[B] =
    futureOption.flatMap(f andThen(twitterToScalaFutureOption(_)))

  def map[B](f: A => B)(implicit ec: ExecutionContext): TwitterFutureOption[B] = futureOption.map(f)

  def filter(p: (A) ⇒ Boolean)(implicit ec: ExecutionContext): TwitterFutureOption[A] = filterWith(p)

  def filterWith(p: (A) ⇒ Boolean)(implicit ec: ExecutionContext): TwitterFutureOption[A] = futureOption.filter(p)

  private def futureOption: FutureOption[A] = FutureOption(future)
}

case class TwitterFutureEither[L, +R](future: twitter.Future[Either[L, R]]) extends AnyVal {
  def flatMap[R2](f: R => TwitterFutureEither[L, R2])(implicit ec: ExecutionContext): TwitterFutureEither[L, R2] =
    futureEither.flatMap(f andThen(twitterToScalaFutureEither(_)))

  def map[R2](f: R => R2)(implicit ec: ExecutionContext): TwitterFutureEither[L, R2] = futureEither.map(f)

  def filter(p: (R) ⇒ Boolean)(implicit ec: ExecutionContext): TwitterFutureEither[String, R] = filterWith(p)

  def filterWith(p: (R) ⇒ Boolean)(implicit ec: ExecutionContext): TwitterFutureEither[String, R] = futureEither.filterWith(p)

  def filterWithDefault(p: (R) ⇒ Boolean, default: L)(implicit ec: ExecutionContext): TwitterFutureEither[L, R] = futureEither.filterWithDefault(p, default)

  private def futureEither: FutureEither[L, R] = FutureEither(future)
}


object TwitterFutureOps {
  implicit def scalaToTwitterTry[T](t: Try[T]): twitter.Try[T] = t match {
    case Success(r) => twitter.Return(r)
    case Failure(ex) => twitter.Throw(ex)
  }

  implicit def twitterToScalaTry[T](t: twitter.Try[T]): Try[T] = t match {
    case twitter.Return(r) => Success(r)
    case twitter.Throw(ex) => Failure(ex)
  }

  implicit def scalaToTwitterFuture[T](f: Future[T])(implicit ec: ExecutionContext): twitter.Future[T] = {
    val promise = twitter.Promise[T]()
    f.onComplete(promise update _)
    promise
  }

  implicit def twitterToScalaFuture[T](f: twitter.Future[T]): Future[T] = {
    val promise = Promise[T]()
    f.respond(promise complete _)
    promise.future
  }

  implicit def twitterToScalaFutureOption[T](f: TwitterFutureOption[T])(implicit ec: ExecutionContext) : FutureOption[T] = {
    FutureOption(f.future)
  }

  implicit def twitterToScalaFutureEither[L, R](f: TwitterFutureEither[L, R])(implicit ec: ExecutionContext) : FutureEither[L, R] = {
    FutureEither(f.future)
  }
  implicit def scalaToTwitterFutureOption[T](f: FutureOption[T])(implicit ec: ExecutionContext) : TwitterFutureOption[T] = {
    TwitterFutureOption(f.future)
  }

  implicit def scalaToTwitterFutureEither[L, R](f: FutureEither[L, R])(implicit ec: ExecutionContext) : TwitterFutureEither[L, R] = {
    TwitterFutureEither(f.future)
  }

}
