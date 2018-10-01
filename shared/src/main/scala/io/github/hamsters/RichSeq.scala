package io.github.hamsters

import scala.language.implicitConversions

case class RichSeq[L,R](l : Seq[Either[L,R]]) {
  def collectRights : Seq[R] = l.collect{ case Right(value) => value}
}

object RichSeq {
  implicit  def toRichList[L,R](l : Seq[Either[L,R]]): RichSeq[L, R] = RichSeq(l)
}