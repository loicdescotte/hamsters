package io.github.hamsters

object ValidationExtensions {

  implicit class RightBiasedEither[L, R](e: Either[L, R]) {
    def map[R2](f: R => R2) = e.right.map(f)

    def flatMap[R2](f: R => Either[L, R2]) = e.right.flatMap(f)

    def filter(p: (R) => Boolean) = filterWith(p)

    def filterWith(p: (R) => Boolean) = e.right.filter(p)

    def get = e.right.get

    def getOrElse[R2 >: R](or: => R2) = e.right.getOrElse(or)
  }

}
