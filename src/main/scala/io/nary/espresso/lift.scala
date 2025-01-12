package io.nary.espresso

import cats.syntax.all.*
import cats.data.Kleisli

import scala.language.postfixOps

object lift:
  import defs.*

  def const[E, A, B](k: B): Expr[E, A, B] =
    Kleisli.apply[[α] =>> Term[E, α], A, B] { _ => k.validNel }

  def error[E, A, B](e: E): Expr[E, A, B] =
    Kleisli.apply[[α] =>> Term[E, α], A, B] { _ => e.invalidNel }

  def funcExpr1[E, A, B](f: A => B)(g: String => E): Expr[E, A, B] =
    Kleisli.apply[[α] =>> Term[E, α], A, B] { a =>
        Either.catchNonFatal(f(a)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr2[E, A1, A2, B](f: (A1, A2) => B)(g: String => E): Expr[E, (A1, A2), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2), B] {
      case (a1, a2) =>
        Either.catchNonFatal(f(a1, a2)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr3[E, A1, A2, A3, B](f: (A1, A2, A3) => B)(g: String => E): Expr[E, (A1, A2, A3), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3), B] {
      case (a1, a2, a3) =>
        Either.catchNonFatal(f(a1, a2, a3)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr4[E, A1, A2, A3, A4, B](f: (A1, A2, A3, A4) => B)(g: String => E): Expr[E, (A1, A2, A3, A4), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4), B] {
      case (a1, a2, a3, a4) =>
        Either.catchNonFatal(f(a1, a2, a3, a4)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr5[E, A1, A2, A3, A4, A5, B](f: (A1, A2, A3, A4, A5) => B)(g: String => E): Expr[E, (A1, A2, A3, A4, A5), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5), B] {
      case (a1, a2, a3, a4, a5) =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr6[E, A1, A2, A3, A4, A5, A6, B](f: (A1, A2, A3, A4, A5, A6) => B)(g: String => E): Expr[E, (A1, A2, A3, A4, A5, A6), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6), B] {
      case (a1, a2, a3, a4, a5, a6) =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr7[E, A1, A2, A3, A4, A5, A6, A7, B](f: (A1, A2, A3, A4, A5, A6, A7) => B)(g: String => E): Expr[E, (A1, A2, A3, A4, A5, A6, A7), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7), B] {
      case (a1, a2, a3, a4, a5, a6, a7) =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr8[E, A1, A2, A3, A4, A5, A6, A7, A8, B](f: (A1, A2, A3, A4, A5, A6, A7, A8) => B)(g: String => E): Expr[E, (A1, A2, A3, A4, A5, A6, A7, A8), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7, A8), B] {
      case (a1, a2, a3, a4, a5, a6, a7, a8) =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7, a8)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr9[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B)(g: String => E): Expr[E, (A1, A2, A3, A4, A5, A6, A7, A8, A9), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7, A8, A9), B] {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9) =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7, a8, a9)).leftMap(e => g(e.getMessage)).toValidatedNel
    }

  def funcExpr10[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B)(g: String => E): Expr[E, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), B] {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =>
        Either.catchNonFatal(f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)).leftMap(e => g(e.getMessage)).toValidatedNel
    }
