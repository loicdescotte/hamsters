package io.github.hamsters

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._
class GenLens extends StaticAnnotation {

  @inline
  def apply(defn: Any): Any = {
    defn match {
      case q"""case class $tName(..$params)""" => {

        val classParameters = params
          .map(p => (p.name, p.decltpe))
          .collect { case (name, Some(declType)) => (name.value, Type.Name(declType.toString())) }

        val clazz = q""" case class $tName(..$params)  """
        val companion =
          q"""object ${Term.Name(tName.value)} {
             ..${
            classParameters.map(classParameter =>
            q"""
               val ${Pat.Var.Term(Term.Name("_" + classParameter._1))} = new  io.github.hamsters.Lens[$tName,${classParameter._2}] {
                  override def get: $tName => ${classParameter._2} = _.${Term.Name(classParameter._1)}
                  override def set: $tName => ${classParameter._2} => $tName =  s => updater  => s.copy(${Term.Name(classParameter._1)}= updater)
               }""")}
              }"""
        val res = Term.Block(Seq(clazz, companion))
        // println(res.syntax)
        res
      }
      case _ =>
        println(defn.structure)
        abort("@GenLens must annotate a case class")
    }
  }
}
