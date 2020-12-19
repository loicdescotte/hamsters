package io.github.hamsters

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

class CartesianMacro extends StaticAnnotation {

  @inline
  def apply(defn: Any): Any = {

    val MAX_ARITY = 23

    // t1 => t._1
    def t(t : Term.Name) = Term.Name(t.value.replace("t", "t._"))

    val names =((1 until MAX_ARITY) map(i => Term.Name(s"t$i"))).toList
    def products(n : List[Term.Name]):Term.Apply = {
      n match {
        case a1 :: a2 :: Nil => q"s.product(${t(a1)}, ${t(a2)}) "
        case a1 :: a2 :: a3 :: Nil => q"s.product(s.product(${t(a1)}, ${t(a2)}), ${t(a3)})"
        case a1 :: a2 :: a3 :: a4 => q"s.product(s.product(${t(a1)}, ${t(a2)}), ${products(a3 :: a4)})"
        case _ => ??? // never happen
      }
    }

    // produce f: (T, T,...) => T
    def fTuples(i : Int) = param"f : (..${(0 until MAX_ARITY).map(_ => targ"T").take(i)}) => T"

    def patTerm(t : Term.Name) = Pat.Var.Term(t)
    def patValue(n : Seq[Term.Name]): Term.Apply = q"f(..$n)"

    // produce (a1, a2),(a3,a4),a5...
    def patKey(n : List[Term.Name]) : Pat.Tuple  = n match {
      case a1 :: a2 :: Nil => p"(${patTerm(a1)}, ${patTerm(a2)})"
      case a1 :: a2 :: a3 :: Nil => p"((${patTerm(a1)}, ${patTerm(a2)}), ${patTerm(a3)})"
      case a1 :: a2 :: a3  :: a4=> p"((${patTerm(a1)}, ${patTerm(a2)}), ${patKey(List(a3) ++ a4)})"
      case _ => ??? // never happen
    }

    def partials(n: List[Term.Name]): Term.PartialFunction = q"{ case (${patKey(n)}) => ${patValue(n)}}"

    //produces t : (Box[T], Box[T],...) with an arity of i
    def boxes(i  : Int): Type.Tuple = {
      val boxes  =   (0 until i) map (_ => t"Box[T]")
      t"(..$boxes)"
    }

    def mapNMethod(i : Int): Defn.Def =
      q"""def mapN(${fTuples(i)})(implicit functor: Functor[Box], s: Cartesian[Box]): Box[T] = {
              functor.map(${products(names.take(i))}){ ${partials(names.take(i))}}
              }"""

    // produces TupleXBox case class  with mapN method
    def tuple(i: Int): Defn.Class = {
      q"""case class ${Type.Name(s"Tuple${i}Box")}[Box[_], T](t: ${boxes(i)}){
             ${mapNMethod(i)}
             }"""
    }

    val tuples = (2 until MAX_ARITY).map(tuple)

    // produce conversions method between tuples of box  ands TupleNBox :
    // ex : implicit def t2x[Box[_], T](t: (Box[T], Box[T])) = Tuple2Box(t)
    def tupleBox2CaseClassBox(i  : Int) = q"""implicit def ${Term.Name(s"t${i}x")}[Box[_], T](t: ${boxes(i)}) = ${Term.Name(s"Tuple${i}Box")}(t)"""

    val implicits = (2 until MAX_ARITY).map(tupleBox2CaseClassBox)

    defn match {
      case q"object $objectName { ..$objectBody }" =>
       q"""object $objectName {
         ..$objectBody
         ..$tuples
         ..$implicits
      }"""

      case _ => abort(defn.pos, "Invalid annottee - you can only use @CartesianMacro on Object")
    }

  }
}
