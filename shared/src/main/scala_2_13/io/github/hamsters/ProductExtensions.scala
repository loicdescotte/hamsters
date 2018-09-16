package io.github.hamsters

object ProductExtensions {

  implicit class ProductShow(p: Product) {
    def show: String =
      p.productElementNames.zip(p.productIterator)
        .map { case (name, value) => s"$name=$value" }
        .mkString(p.productPrefix + "(", ", ", ")")
  }

}