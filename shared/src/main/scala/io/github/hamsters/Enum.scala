package io.github.hamsters

trait Enumerable[A] {

  def name(a: A): String = Showable.fromToStringLowerCase.format(a)

  def parse(s: String): Option[A] = list.find(a => name(a) == s)

  def list: List[A]
}

object Enumeration {

  /**
   * String name for an Enumerable object
   * @param a
   * @param ev
   * @tparam A
   * @return name
   */
  def name[A](a: A)(implicit ev: Enumerable[_ >: A]): String = ev.name(a)

  /**
   * Resolve Option of Enumerable object from String
   * @param s
   * @param ev
   * @tparam A
   * @return Enumerable object
   */
  def parse[A](s: String)(implicit ev: Enumerable[_ <: A]): Option[A] = ev.parse(s)

  /**
   * List Enumerable objects for an Enumerable type
   * @param ev
   * @tparam A
   * @return List of Enumerable objects
   */
  def list[A](implicit ev: Enumerable[A]): List[A] = ev.list
}
