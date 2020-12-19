package io.github.hamsters

import scala.collection.immutable.Seq
import scala.meta._

class ShowMacro extends scala.annotation.StaticAnnotation {

  @inline
  def apply(defn: Any): Any = {
    defn match {
      // TODO handle object with existing companion
      // case  Seq(cls @ Defn.Class(_, name, _, ctor, template), companion: Defn.Object)=>  q""""""
      case cls @ Defn.Class(_, _, _, ctor, _) =>

        val show =
          q"""
        implicit def showable  = new io.github.hamsters.Showable[${cls.name}] { override def format(a: ${cls.name}) ={
        import io.github.hamsters.ShowableSyntax._
             ${Lit.String(cls.name.value)}+ "(" + List(..${ctor.paramss.flatMap(_.map(pp => q"""${pp.name.syntax} + "=" + Show.show(a.${Term.Name(pp.name.value)})""" ))}).reduce(_ + "," + _) + ")"    }   }"""
        val companion = q"object ${Term.Name(cls.name.value)} { $show }"

        val res = Term.Block(Seq(cls, companion))
        // abort(res.syntax)
        res
      case _ => abort(defn.pos, "Invalid annottee - you can only use @ShowMacro on case classes")
    }
  }

}
