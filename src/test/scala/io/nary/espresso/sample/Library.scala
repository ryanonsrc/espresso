package io.nary.espresso.sample

import io.nary.espresso.defs._
import io.nary.espresso.compose._
import io.nary.espresso.lift._
import shapeless.{::, HNil}
import cats.syntax.all._

object Library {
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

}