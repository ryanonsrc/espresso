package io.nary.espresso.sample

import io.nary.espresso.defs.*
import io.nary.espresso.compose.*
import io.nary.espresso.lift.*
import shapeless3.typeable.*
import shapeless3.data.{::, HNil}
import cats.syntax.all.*

object Library:
  /** Concatenates two strings */
  def concat[E]: Expr[E, String :: String :: HNil, String] =
    op2[E, String, String, String] {
      case l :: r :: HNil =>
        (l, r).mapN(_ + _)
    }

  /** Reverses a string */
  def reverse[E]: Expr[E, String :: HNil, String] =
    op1[E, String, String] {
      case s :: HNil =>
        s.map(_.reverse)
    }

  def caps[E]: Expr[E, String :: HNil, String] =
    op1[E, String, String] {
      case s :: HNil =>
        s.map(_.toUpperCase)
    }

  def lower[E]: Expr[E, String :: HNil, String] =
    op1[E, String, String] {
      case s :: HNil =>
        s.map(_.toLowerCase)
    }

  def asInteger[E, A](err: String => E): Expr[E, String :: HNil, Int] =
    funcExpr1[E, String, Int](_.toInt)(err)

  def asString[E, A](err: String => E): Expr[E, Int :: HNil, String] =
    funcExpr1[E, Int, String](_.toString)(err)

  def mapExpr[E, A, B](err: String => E)(f: A => B): Expr[E, A :: HNil, B] =
    funcExpr1[E, A, B](f)(err)