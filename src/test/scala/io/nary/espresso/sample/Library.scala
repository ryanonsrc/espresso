package io.nary.espresso.sample

import io.nary.espresso.defs.*
import io.nary.espresso.compose.*
import io.nary.espresso.lift.*
import cats.syntax.all.*

object Library:
  /** Concatenates two strings */
  def concat[E]: Expr[E, (String, String), String] =
    op2[E, String, String, String] {
      case (l, r) => (l, r).mapN((_ + _))
    }

  /** Reverses a string */
  def reverse[E]: Expr[E, String, String] =
    op1[E, String, String](_.map(_.reverse))

  def caps[E]: Expr[E, String, String] =
    op1[E, String, String] (_.map(_.toUpperCase))

  def lower[E]: Expr[E, String, String] =
    op1[E, String, String] (_.map(_.toLowerCase))

  def asInteger[E, A](err: String => E): Expr[E, String, Int] =
    funcExpr1[E, String, Int](_.toInt)(err)

  def asString[E, A](err: String => E): Expr[E, Int, String] =
    funcExpr1[E, Int, String](_.toString)(err)

  def mapExpr[E, A, B](err: String => E)(f: A => B): Expr[E, A, B] =
    funcExpr1[E, A, B](f)(err)