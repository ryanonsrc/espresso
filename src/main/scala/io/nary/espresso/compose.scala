package io.nary.espresso

import cats.Applicative

import cats.syntax.all._
import cats.std.all._

import cats.data.Kleisli
import cats.data.{Kleisli, Validated, ValidatedNel, Xor, NonEmptyList}
import cats.data.Validated.{Invalid, Valid}

import shapeless.{::, HNil}

object compose {
  import defs._

  def unaryOp[E, O, A](f: (Term[E, O] :: HNil) => Term[E, A]) : Expr[E, O :: HNil, A] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, O :: HNil, A] { case op :: HNil =>
      f(Applicative[({type λ[α] = Term[E, α]})#λ].pure(op) :: HNil)
    }

  def binaryOp[E, L, R, A](f: (Term[E, L] :: Term[E, R] :: HNil) => Term[E, A]) : Expr[E, L :: R :: HNil, A] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, L :: R :: HNil, A] { case l :: r :: HNil =>
      f(Applicative[({type λ[α] = Term[E, α]})#λ].pure(l) :: Applicative[({type λ[α] = Term[E, α]})#λ].pure(r) :: HNil)
    }

  def ternaryOp[E, X, Y, Z, A](f: (Term[E, X] :: Term[E, Y] :: Term[E, Z] :: HNil) => Term[E, A]) : Expr[E, X :: Y :: Z :: HNil, A] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, X :: Y :: Z :: HNil, A] { case x :: y :: z :: HNil =>
      f(Applicative[({type λ[α] = Term[E, α]})#λ].pure(x) :: Applicative[({type λ[α] = Term[E, α]})#λ].pure(y) :: Applicative[({type λ[α] = Term[E, α]})#λ].pure(z) :: HNil)
    }

  def unaryJoin[E, A, O](operand: Expr[E, A, O] :: HNil) : Expr[E, A, O :: HNil] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, A, O :: HNil] { a => operand match {
      case op :: HNil => op.run(a).map(_ :: HNil)
    }
  }

  def binaryJoin[E, A, L, R](operands: Expr[E, A, L] :: Expr[E, A, R] :: HNil) : Expr[E, A, L :: R :: HNil] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, A, L :: R :: HNil] { a => operands match {
      case l :: r :: HNil => (l.run(a) |@| r.run(a)).map(_ :: _ :: HNil)
    }
  }

  def ternaryJoin[E, A, X, Y, Z, R](operands: Expr[E, A, X] :: Expr[E, A, Y] :: Expr[E, A, Z] :: HNil) : Expr[E, A, X :: Y :: Z :: HNil] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, A, X :: Y :: Z :: HNil] { a => operands match {
      case x :: y :: z :: HNil => (x.run(a) |@| y.run(a) |@| z.run(a)).map(_ :: _ :: _ :: HNil)
    }
  }

  def unaryEval[E, A, B, C](operand: Expr[E, A, B], op: Expr[E, B :: HNil, C]) : Expr[E, A, C] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, A, C] { source => unaryJoin(operand :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(v :: HNil) => op.run(v :: HNil)
    }}

  def binaryEval[E, A, B, C, D](l: Expr[E, A, B], r: Expr[E, A, C], op: Expr[E, B :: C :: HNil, D]) : Expr[E, A, D] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, A, D] { source => binaryJoin(l :: r :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(lv :: rv :: HNil) => op.run(lv :: rv :: HNil)
    }}

  def ternaryEval[E, V, W, X, Y, Z](V: Expr[E, V, W], y: Expr[E, V, X], z: Expr[E, V, Y], op: Expr[E, W :: X :: Y :: HNil, Z]) : Expr[E, V, Z] =
    Kleisli[({type λ[α] = Term[E, α]})#λ, V, Z] { source => ternaryJoin(V :: y :: z :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(xv :: yv :: zv :: HNil) => op.run(xv :: yv :: zv :: HNil)
    }}

}
