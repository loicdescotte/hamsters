package io.github.hamsters

import scala.collection.immutable.Seq
import scala.meta._

class HListMacro extends scala.annotation.StaticAnnotation {

  @inline
  def apply(defn: Any): Any = {

    defn match {
      case q"..$mods class $tName(..$params)" =>

        val hlistParams = params.map(param => param.name.value)
                                .foldRight("HNil")((term1, term2) => s"value.${term1}::${term2}")
                                .parse[Term]
                                .get

        val hlistType = params.map(param => MetaHelper.toType(param.decltpe.get))
                              .foldRight("HNil")((type1, type2) => s"${type1}::${type2}")
                              .parse[Type]
                              .get


        val constructor = params.zipWithIndex
                        .map { case (param : Term.Param, index : Int) =>
                              val tpe = MetaHelper.toType(param.decltpe.get)
                              val term = Term.Name(param.name.value)
                              q"${term} = hlist.apply[$tpe](${Lit.Int(index)})"
                        }

        val fcts = q"""
              import io.github.hamsters._
              import HList._
              implicit def toHList(value : $tName) : $hlistType = ${hlistParams}
              implicit def toClass(hlist : HList) : ${tName.tpe} = ${Term.Name(tName.value)}(..${constructor})
          """

      val companion = MetaHelper.getOrCreateCompanion(defn)
      val updatedCompanion = companion.copy(templ = companion.templ.copy(stats = Some(fcts.stats ++ companion.templ.stats.getOrElse(Nil))))

      q"""..$mods class $tName(..$params)
           $updatedCompanion
         """
      case _ =>
        println(defn.structure)
        abort("@HListMacro must annotate a class")
    }
  }
}
