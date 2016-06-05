package io.nary.espresso.sample

import cats.syntax.all._
import cats.data.Kleisli
import io.nary.espresso.adapters.readers
import io.nary.espresso.{compose, defs}

object Syntax {
  import defs._
  import compose._
  import readers._
  import Library._

  type Key = String
  type Source = Map[Key, Any] // Weakly-typed Source

  case class EvalError(msg: String)

  implicit class StrExpr(val l: Expr[EvalError, Source, String]) extends AnyVal {
    def &(r: Expr[EvalError, Source, String]) : Expr[EvalError, Source,String] = binaryEval(l, r, concat)
    def rev : Expr[EvalError, Source, String] = unaryEval(l, reverse)
  }

  def strF(k: Key) =
    read[EvalError, String, Key, Source](k, EvalError(s"key missing: $k"),
      (s: Source) =>
        (k: Key) =>
          (in: In[EvalError, String]) =>
            s.get(k) map in.run)


  implicit def strReader1(k: Key) : StrExpr = strF(k)
  implicit def strReader2(k: Key) : Expr[EvalError, Source, String] = strF(k)

  implicit def const[A](k: A) : Expr[EvalError, Source, A] =
    Kleisli[({type λ[α] = Term[EvalError, α]})#λ, Source, A] { _ =>
      k.validNel
  }

}
