package io.github.hamsters

import RichSeq._
import org.scalatest.{FlatSpec, Matchers}
class RichSeqSpec extends FlatSpec with Matchers {

  "seq collectRights" should "only collect the rights values" in {
   Seq(Left("error1"), Right(1)).collectRights shouldBe List(1)
  }

  "list collectRights" should "only collect the rights values" in {
    List(Left("error1"), Right(1)).collectRights shouldBe List(1)
  }

}
