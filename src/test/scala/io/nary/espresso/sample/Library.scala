package io.nary.espresso.sample

import io.nary.espresso.defs._
import io.nary.espresso.compose._
import shapeless.{::, HNil}

import cats.syntax.all._
import cats.std.all._

object Library {
  def concat[E]: Expr[E, String :: String :: HNil, String] = binaryOp[E, String, String, String] {
    case l :: r :: HNil => (l |@| r).map {_ + _}}

  def reverse[E]: Expr[E, String :: HNil, String] = unaryOp[E, String, String] {
    case s :: HNil => s.map(_.reverse) }
}
