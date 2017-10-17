package io.github.hamsters

/**
  * Created by psachathamak on 10/17/2017 AD.
  */
object HString {
  implicit class HStringHelper(optStr: Option[String]){
    def orEmpty: String = optStr.getOrElse("")
  }
}
