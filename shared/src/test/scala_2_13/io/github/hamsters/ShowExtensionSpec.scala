package io.github.hamsters

import org.scalatest.{FlatSpec, Matchers}

class ShowExtensionSpec extends FlatSpec with Matchers {

  import ProductExtensions._

  case class Name(firstName: String, lastName: String)
  case class Person(name: Name, age: Int)

  "Show on simple object" should "show field names and values of object" in {
    val n = Name("john", "doe")
    n.show should be("Name(firstName=john,lastName=doe)")
  }

}