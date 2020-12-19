package io.github.hamsters

import scala.collection.immutable.Seq
import scala.meta.Term.Name
import scala.meta.{Term, _}

class UnionMacro extends scala.annotation.StaticAnnotation {

  @inline
  def apply(defn: Any): Any = {

    def argByXY(idx: Int, i: Int): Term =
      if (idx == i) q"""Some(t)"""
      else q"""None"""

    defn match {
      case q"class $className[..$classTypes] { ..$body }" =>
        val types = classTypes.map(t => Type.Name(t.name.value))
        val typesName: Seq[Name] = classTypes.map(t => Term.Name(t.name.value))
        val result =
          q"""
        class $className[..$classTypes] {
        ..$body
         ..${

            typesName.zipWithIndex.map { case (currentTypeName, y) =>
              val unionXType2UnionXMethodName = Term.Name("toUnion" + y)
              val currentType = Type.Name(currentTypeName.value)
              val unionXType = Term.Name("Union" + classTypes.size)
              val constructorArgs : Seq[Term.Arg] = for (x <- typesName.indices) yield q"""${argByXY(x, y)}"""
              q"implicit def $unionXType2UnionXMethodName(t : $currentType) = $unionXType[..$types](..$constructorArgs)"
            }
          }
        }
          """
        // println(result.syntax) display code result
        result
      case _ =>
        println(defn.structure)
        abort("@UnionMacroMeta must annotate a class")
    }
  }
}
