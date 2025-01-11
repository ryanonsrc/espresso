package io.nary.espresso.sample

import cats.syntax.all._
import cats.data.Kleisli
import io.nary.espresso.adapters.readers
import io.nary.espresso.{compose, defs, lift}

object Syntax {
  import defs._
  import compose._
  import readers._
  import lift._
  import Library._

  type Key = String
  type Source = Map[Key, Any] // Weakly-typed Source

  case class EvalError(msg: String)

  implicit class LookupExpr(val l: Expr[EvalError, Source, String]) extends AnyVal {
    def &(r: Expr[EvalError, Source, String]) : Expr[EvalError, Source, String] = eval2(l, r, concat)
    def rev : Expr[EvalError, Source, String] = eval1(l, reverse)
    def unary_+ : Expr[EvalError, Source, String] = eval1(l, caps)
    def unary_- : Expr[EvalError, Source, String] = eval1(l, lower)
    def asInt : Expr[EvalError, Source, Int] = eval1(l, asInteger(EvalError))
  }

  implicit class IntExpr(val l: Expr[EvalError, Source, Int]) extends AnyVal {
    def asStr : Expr[EvalError, Source, String] = eval1(l, asString(EvalError))
  }

  def strF(k: Key) =
    read[EvalError, String, Key, Source](k, EvalError(s"key missing: $k"),
      (s: Source) =>
        (k: Key) =>
          (in: In[EvalError, String]) =>
            s.get(k) map in.run)

  def str(s: String) : Expr[EvalError, Source, String] = const(s)

  implicit def strReader1(k: Key) : LookupExpr = strF(k)
  implicit def strReader2(k: Key) : Expr[EvalError, Source, String] = strF(k)
}
