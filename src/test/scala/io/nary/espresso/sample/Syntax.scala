package io.nary.espresso.sample

import cats.syntax.all.*
import cats.data.Kleisli
import io.nary.espresso.adapters.readers
import io.nary.espresso.{compose, defs, lift}

object Syntax:
  import defs.*
  import compose.*
  import readers.*
  import readers.given 
  import lift.*
  import Library.*

  type Key = String
  type Source = Map[Key, Any] // Weakly-typed Source

  case class EvalError(msg: String)

  extension (l: Expr[EvalError, Source, String])
    def &(r: Expr[EvalError, Source, String]): Expr[EvalError, Source, String] =
      eval2(l, r, concat)
    def rev: Expr[EvalError, Source, String] =
      eval1(l, reverse)
    def unary_+ : Expr[EvalError, Source, String] =
      eval1(l, caps)
    def unary_- : Expr[EvalError, Source, String] =
      eval1(l, lower)
    def asInt: Expr[EvalError, Source, Int] =
      eval1(l, asInteger(EvalError.apply))
    def ^=>(f: String => String): Expr[EvalError, Source, String] =
      eval1(l, mapExpr(EvalError.apply)(f))

  extension (l: Expr[EvalError, Source, Int])
    def asStr: Expr[EvalError, Source, String] =
      eval1(l, asString(EvalError.apply))

  def strF(k: Key): Expr[EvalError, Source, String] =
    read[EvalError, String, Key, Source](
      k,
      EvalError(s"key missing: $k"),
      (s: Source) =>
        (k: Key) =>
          (in: In[EvalError, String]) =>
            s.get(k) map in.run
    )

  def str(s: String): Expr[EvalError, Source, String] = const(s)
  
  given Conversion[Key, Expr[EvalError, Source, String]] = strF(_)