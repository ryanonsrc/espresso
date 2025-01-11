package io.nary.espresso

import cats.Applicative
import cats.syntax.all.*
import cats.data.Kleisli
import cats.data.Validated.{Invalid, Valid}
import shapeless3.typeable.*
import shapeless3.data.{::, HNil}

object lift:
  import defs.*
  import compose.*

  def const[E, A, B](k: B): Expr[E, A, B] =
    Kleisli.apply[[α] =>> Term[E, α], A, B] { _ => k.validNel }

  def error[E, A, B](e: E): Expr[E, A, B] =
    Kleisli.apply[[α] =>> Term[E, α], A, B] { _ => e.invalidNel }

  def funcExpr1[E, A1, B](f: (A1) => B)(g: String => E): Expr[E, A1 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: HNil, B] {
      case (a1: A1) :: HNil =>
        Either.catchNonFatal(f(a1)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr2[E, A1, A2, B](f: (A1, A2) => B)(g: String => E): Expr[E, A1 :: A2 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: HNil =>
        Either.catchNonFatal(f(a1, a2)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr3[E, A1, A2, A3, B](f: (A1, A2, A3) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr4[E, A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: A4 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: A4 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: (a4: A4) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3, a4)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr5[E, A1, A2, A3, A4, A5, B](f: (A1, A2, A3, A4, A5) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: A4 :: A5 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: (a4: A4) :: (a5: A5) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr6[E, A1, A2, A3, A4, A5, A6, B](f: (A1, A2, A3, A4, A5, A6) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: (a4: A4) :: (a5: A5) :: (a6: A6) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr7[E, A1, A2, A3, A4, A5, A6, A7, B](f: (A1, A2, A3, A4, A5, A6, A7) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: (a4: A4) :: (a5: A5) :: (a6: A6) :: (a7: A7) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr8[E, A1, A2, A3, A4, A5, A6, A7, A8, B](f: (A1, A2, A3, A4, A5, A6, A7, A8) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: (a4: A4) :: (a5: A5) :: (a6: A6) :: (a7: A7) :: (a8: A8) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7, a8)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr9[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: (a4: A4) :: (a5: A5) :: (a6: A6) :: (a7: A7) :: (a8: A8) :: (a9: A9) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7, a8, a9)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr10[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B)(g: String => E): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: HNil, B] =
    Kleisli.apply[[α] =>> Term[E, α], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: HNil, B] {
      case (a1: A1) :: (a2: A2) :: (a3: A3) :: (a4: A4) :: (a5: A5) :: (a6: A6) :: (a7: A7) :: (a8: A8) :: (a9: A9) :: (a10: A10) :: HNil =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)).leftMap(e => g(e.getMessage)).toValidatedNel
    }
