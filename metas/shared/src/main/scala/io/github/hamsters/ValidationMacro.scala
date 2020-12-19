package io.github.hamsters

import scala.collection.immutable
import scala.meta._

class ValidationMacro extends scala.annotation.StaticAnnotation {

  @inline
  def apply(defn: Any): Any = {

    val arityRange = Range(1, 23)

    val typeParams: immutable.Seq[Type.Param] = arityRange.map(i => Type.Name("R" + i)).map(n => tparam"..$Nil $n")
    val typeNames = typeParams map (n => Type.Name(n.name.value))

    val params = arityRange map (i => param"""${Term.Name(s"e$i")} : ${t"""Either[L, ${Type.Name(s"R$i")}]"""} """)
    defn match {
      case q"object $t { ..$body }" =>
        val resul =
          q"""
           object $t {
            ..$body
             import scala.util.Try
             import scala.util.Success
             import scala.util.Failure
             implicit def fromTry[T](t : Try[T]): Either[String,T] = {
               t match {
                 case Success(res) => Right(res)
                 case Failure(e) => Left(e.getMessage)
               }
             }
            ..${
            Range(3, 22).map { index =>
              q"""def run[L,..${typeParams.take(index + 1)}](..${params.take(index + 1)}) : Either[List[L], (..${typeNames.take(index + 1)})] = {
                failures(..${params.take(index + 1).map(p => Term.Name(p.name.value))}) match {
                   case Nil => Right(..${params.take(index + 1).map(p =>q"""${Term.Name(p.name.value)}.get""")})
                   case f : List[L] => Left(f)
                }
              }"""
            }
          }
           }
         """
//        abort(resul.syntax)
        resul
    }
  }
}
