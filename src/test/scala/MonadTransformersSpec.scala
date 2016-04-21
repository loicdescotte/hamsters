import java.util.concurrent.TimeUnit

import io.github.hamsters.{FutureEither, FutureOption, TwitterFutureEither, TwitterFutureOption}
import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import com.twitter.{util => twitter}

class MonadTransformersSpec extends FlatSpec with Matchers {

  "FutureOption" should "handle Future[Option[_]] type" in {
    def foa: Future[Option[String]] = Future(Some("a"))
    def fob(a: String): Future[Option[String]] = Future(Some(a+"b"))

    val composedAB: Future[Option[String]] = (for {
      a <- FutureOption(foa)
      ab <- FutureOption(fob(a))
    } yield ab).future

    Await.result(composedAB, 1 second) shouldBe Some("ab")

    val composedABWithNone: Future[Option[String]] = (for {
      a <- FutureOption(Future.successful(None))
      ab <- FutureOption(fob(a))
    } yield ab).future

    Await.result(composedABWithNone, 1 second) shouldBe None

    val composedABWithFailure: Future[Option[String]] = (for {
      a <- FutureOption(Future.failed(new Exception("d'oh!")))
      ab <- FutureOption(fob(a))
    } yield ab).future

    an [Exception] should be thrownBy Await.result(composedABWithFailure, 1 second)
  }

  "TwitterFutureOption" should "handle com.twitter.util.Future[Option[_]] type" in {

    def foa: twitter.Future[Option[String]] = twitter.Future(Some("a"))
    def fob(a: String): twitter.Future[Option[String]] = twitter.Future(Some(a+"b"))

    val composedAB: twitter.Future[Option[String]] = (for {
      a <- TwitterFutureOption(foa)
      ab <- TwitterFutureOption(fob(a))
    } yield ab).future

    twitter.Await.result(composedAB, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe Some("ab")

    val composedABWithNone: twitter.Future[Option[String]] = (for {
      a <- TwitterFutureOption(twitter.Future.value(None))
      ab <- TwitterFutureOption(fob(a))
    } yield ab).future

    twitter.Await.result(composedABWithNone, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe None

    val composedABWithFailure: twitter.Future[Option[String]] = (for {
      a <- TwitterFutureOption(twitter.Future.exception(new Exception("d'oh!")))
      ab <- TwitterFutureOption(fob(a))
    } yield ab).future

    an [Exception] should be thrownBy twitter.Await.result(composedABWithFailure, twitter.Duration(1, TimeUnit.SECONDS))
  }

  "FutureOption" should "be filtered with pattern matching in for comprehension" in {
    def fo: Future[Option[(String, Int)]] = Future(Some(("a", 42)))

    val filtered = (for {
      (a, i) <- FutureOption(fo) if i > 5
    } yield a).future

    Await.result(filtered, 1 second) shouldBe Some(("a"))

    val filtered2 = (for {
      (a, i) <- FutureOption(fo) if i > 50
    } yield a).future

    Await.result(filtered2, 1 second) shouldBe None
  }

  "TwitterFutureOption" should "be filtered with pattern matching in for comprehension" in {
    def fo: twitter.Future[Option[(String, Int)]] = twitter.Future(Some(("a", 42)))

    val filtered = (for {
      (a, i) <- TwitterFutureOption(fo) if i > 5
    } yield a).future

    twitter.Await.result(filtered, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe Some(("a"))

    val filtered2 = (for {
      (a, i) <- TwitterFutureOption(fo) if i > 50
    } yield a).future

    twitter.Await.result(filtered2, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe None
  }

  "FutureEither" should "handle Future[Either[_,_]] type" in {
    def fea: Future[Either[String, Int]] = Future(Right(1))
    def feb(a: Int): Future[Either[String, Int]] = Future(Right(a+2))

    val composedAB: Future[Either[String, Int]] = (for {
      a <- FutureEither(fea)
      ab <- FutureEither(feb(a))
    } yield ab).future

    Await.result(composedAB, 1 second) shouldBe Right(3)

    val composedABWithNone: Future[Either[String, Int]] = (for {
      a <- FutureEither(Future.successful(Left("d'oh!")))
      ab <- FutureEither(feb(a))
    } yield ab).future

    Await.result(composedABWithNone, 1 second) shouldBe Left("d'oh!")

    val composedABWithFailure: Future[Either[String, Int]] = (for {
      a <- FutureEither(Future.failed(new Exception("d'oh!")))
      ab <- FutureEither(feb(a))
    } yield ab).future

    an [Exception] should be thrownBy Await.result(composedABWithFailure, 1 second)
  }

  "TwitterFutureEither" should "handle com.twitter.util.Future[Either[_,_]] type" in {
    def fea: twitter.Future[Either[String, Int]] = twitter.Future(Right(1))
    def feb(a: Int): twitter.Future[Either[String, Int]] = twitter.Future(Right(a+2))

    val composedAB: twitter.Future[Either[String, Int]] = (for {
      a <- TwitterFutureEither(fea)
      ab <- TwitterFutureEither(feb(a))
    } yield ab).future

    twitter.Await.result(composedAB, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe Right(3)

    val composedABWithNone: twitter.Future[Either[String, Int]] = (for {
      a <- TwitterFutureEither(twitter.Future.value(Left("d'oh!")))
      ab <- TwitterFutureEither(feb(a))
    } yield ab).future

    twitter.Await.result(composedABWithNone, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe Left("d'oh!")

    val composedABWithFailure: twitter.Future[Either[String, Int]] = (for {
      a <- TwitterFutureEither(twitter.Future.exception(new Exception("d'oh!")))
      ab <- TwitterFutureEither(feb(a))
    } yield ab).future

    an [Exception] should be thrownBy twitter.Await.result(composedABWithFailure, twitter.Duration(1, TimeUnit.SECONDS))
  }

  "FutureEither" should "be filtered with pattern matching in for comprehension" in {
    def fe: Future[Either[String, (String, Int)]] = Future(Right(("a", 42)))

    val filtered = (for {
      (a, i) <- FutureEither(fe) if i > 5
    } yield a).future

    Await.result(filtered, 1 second) shouldBe Right("a")

    val filtered2 = (for {
      (a, i) <- FutureEither(fe) if i > 50
    } yield a).future

    Await.result(filtered2, 1 second) shouldBe Left("No value matching predicate")
  }

  "TwitterFutureEither" should "be filtered with pattern matching in for comprehension" in {
    def fe: twitter.Future[Either[String, (String, Int)]] = twitter.Future(Right(("a", 42)))

    val filtered = (for {
      (a, i) <- TwitterFutureEither(fe) if i > 5
    } yield a).future

    twitter.Await.result(filtered, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe Right("a")

    val filtered2 = (for {
      (a, i) <- TwitterFutureEither(fe) if i > 50
    } yield a).future

    twitter.Await.result(filtered2, twitter.Duration(1, TimeUnit.SECONDS)) shouldBe Left("No value matching predicate")
  }
}