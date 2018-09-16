package io.github.hamsters

import scala.util.{Left, Right}

@ValidationMacro
object Validation {

  val Valid = Right
  val Invalid = Left

  /**
    * Return Either from code block
    * @param body
    * @tparam R
    * @return Either from code block
    */
  def fromCatchable[R](body: => R): Either[String, R] = {
    val throwableToErrorMessage = (t: Throwable) => Option(t.getMessage).getOrElse("Error: an exception has been thrown")
    fromCatchable(body, throwableToErrorMessage)
  }

  /**
    * Return Either from code block with custom error management
    * @param body
    * @param errorMgt : error management function
    * @tparam L
    * @tparam R
    * @return Either from code block
    */
  def fromCatchable[L, R](body: => R, errorMgt: (Throwable) => L): Either[L, R] = {
    try {
      Right(body)
    }
    catch {
      case t: Throwable => Left(errorMgt(t))
    }
  }

  /** Return successes (right) for several Either values
    * Validation.run should be used instead in most cases as it is a type-safer method (it does not return a List[Any])
    * @param eithers
    * @return successes
    */
  def successes(eithers : Either[_, _]*) : List[Any]= {
    eithers.toList.collect {
      case r : Right[_, _] => r.right.get
    }
  }

  /**
    * Tells if eithers contain successes (right)
    * @param eithers
    * @tparam R
    * @return boolean
    */
  def hasSuccesses[R](eithers: Either[_, R]*): Boolean = successes(eithers: _*).nonEmpty

  /**
    * Run the validation
    * This validation accumulates all errors
    * Arity 2 to 22 methods are available and generated from macro
    * @param e1 : first either
    * @param e2 : second either
    * @tparam L
    * @tparam R1
    * @tparam R2
    * @return a tuple (R1, R2) of results in a Right if validation is successful or a list of errors L if it fails
    */
  def run[L, R1, R2](e1: Either[L, R1], e2: Either[L, R2]): Either[List[L], (R1, R2)] = {
    failures(e1, e2) match {
      case Nil => Right(e1.get, e2.get)
      case f: List[L] => Left(f)
    }
  }

  /**
    * Run the validation
    * This validation accumulates all errors
    * Arity 2 to 22 methods are available and generated from macro
    * @param e1 : first either
    * @param e2 : second either
    * @param e3 : third either
    * @tparam L
    * @tparam R1
    * @tparam R2
    * @tparam R3
    * @return a tuple (R1, R2, R3) of results in a Right if validation is successful or a list of errors L if it fails
    */
  def run[L, R1, R2, R3](e1: Either[L, R1], e2: Either[L, R2], e3: Either[L, R3]): Either[List[L], (R1, R2, R3)] = {
    failures(e1, e2, e3) match {
      case Nil => Right(e1.get, e2.get, e3.get)
      case f: List[L] => Left(f)
    }
  }

  /**
    * Retrieve failures (left) for several Either values
    * @param eithers
    * @tparam L
    * @return failures
    */
  def failures[L](eithers: Either[L, _]*): List[L] = eithers.toList.collect { case l: Left[L, _] => l.left.get }

  /**
    * Tells if eithers contain failures (left)
    * @param eithers
    * @tparam L
    * @return boolean
    */
  def hasFailures[L](eithers: Either[L, _]*): Boolean = failures(eithers: _*).nonEmpty

}
