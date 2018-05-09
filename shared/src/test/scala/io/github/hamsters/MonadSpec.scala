package io.github.hamsters

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

import scala.language.higherKinds
import scala.reflect.ClassTag

class OptionMonadSpec extends MonadSpec[String, String, String, Option](Monad.optionMonad)
class ListMonadSpec extends MonadSpec[String, String, String, List](Monad.listMonad)

//exceptions break the laws for future
//class FutureMonadSpec extends MonadSpec[String, String, String, Future](Monad.futureMonad)

abstract class MonadSpec[A: Arbitrary, B, C, Box[_]](monad: Monad[Box])
(implicit boxArb: Arbitrary[Box[A]], aToBoxBArb: Arbitrary[A => Box[B]], bToBoxCArb: Arbitrary[B => Box[C]], tag: ClassTag[Box[_]])

extends Properties(s"Monad for $tag") {

  property("left identity") = forAll { (a: A, f: (A => Box[B])) =>
    val boxA: Box[A] = monad.pure(a)
    monad.flatMap(boxA)(f) == f(a)
  }

  property("right identity") = forAll { (a: A, f: (A => Box[B])) =>
    val boxFa: Box[Box[B]] =  monad.pure(f(a))
    monad.flatMap(boxFa)(monad.pure) == boxFa
  }

  property("associativity") = forAll { (box: Box[A], f: (A => Box[B]), g: (B => Box[C])) =>
    val boxB: Box[B] = monad.flatMap(box)(f)
    monad.flatMap(boxB)(g) == monad.flatMap(box) { a =>
      val boxB: Box[B] = f(a)
      monad.flatMap(boxB)(g)
    }
  }

}