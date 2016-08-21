package io.nary.espresso.sample

import io.nary.espresso.defs._
import io.nary.espresso.compose._
import shapeless.{::, HList, HNil}
import cats.syntax.all._
import cats.std.all._

object Library {
  def concat[E]: Expr[E, String :: String :: HNil, String] = op2[E, String, String, String] {
    case l :: r :: HNil ⇒ (l |@| r).map {_ + _}}

  def reverse[E]: Expr[E, String :: HNil, String] = op1[E, String, String] {
    case s :: HNil ⇒ s.map(_.reverse) }
}
