package io.nary.espresso

import cats.Applicative

import cats.syntax.all._
import cats.std.all._
import cats.data.{Kleisli, NonEmptyList, Validated, ValidatedNel, Xor}
import cats.data.Validated.{Invalid, Valid}
import shapeless.{::, HNil}

object compose {
  import defs._

  def op1[E, O, A](f: (Term[E, O] :: HNil) ⇒ Term[E, A]) : Expr[E, O :: HNil, A] =
    Kleisli[λ[α ⇒ Term[E, α]], O :: HNil, A] { case op :: HNil ⇒
      f(Applicative[λ[α ⇒ Term[E, α]]].pure(op) :: HNil)
    }

  def op2[E, L, R, A](f: (Term[E, L] :: Term[E, R] :: HNil) ⇒ Term[E, A]) : Expr[E, L :: R :: HNil, A] =
    Kleisli[λ[α ⇒ Term[E, α]], L :: R :: HNil, A] { case l :: r :: HNil ⇒
      f(Applicative[λ[α ⇒ Term[E, α]]].pure(l) :: Applicative[λ[α ⇒ Term[E, α]]].pure(r) :: HNil)
    }

  def op3[E, X, Y, Z, A](f: (Term[E, X] :: Term[E, Y] :: Term[E, Z] :: HNil) ⇒ Term[E, A]) : Expr[E, X :: Y :: Z :: HNil, A] =
    Kleisli[λ[α ⇒ Term[E, α]], X :: Y :: Z :: HNil, A] { case x :: y :: z :: HNil ⇒
      f(Applicative[λ[α ⇒ Term[E, α]]].pure(x) :: Applicative[λ[α ⇒ Term[E, α]]].pure(y) :: Applicative[({type λ[α] = Term[E, α]})#λ].pure(z) :: HNil)
    }

  def join1[E, A, O](operand: Expr[E, A, O] :: HNil) : Expr[E, A, O :: HNil] =
    Kleisli[λ[α ⇒ Term[E, α]], A, O :: HNil] { a ⇒ operand match {
      case op :: HNil ⇒ op.run(a).map(_ :: HNil)
    }
  }

  def join2[E, A, L, R](operands: Expr[E, A, L] :: Expr[E, A, R] :: HNil) : Expr[E, A, L :: R :: HNil] =
    Kleisli[λ[α ⇒ Term[E, α]], A, L :: R :: HNil] { a ⇒ operands match {
      case l :: r :: HNil ⇒ (l.run(a) |@| r.run(a)).map(_ :: _ :: HNil)
    }
  }

  def join3[E, A, X, Y, Z, R](operands: Expr[E, A, X] :: Expr[E, A, Y] :: Expr[E, A, Z] :: HNil) : Expr[E, A, X :: Y :: Z :: HNil] =
    Kleisli[λ[α ⇒ Term[E, α]], A, X :: Y :: Z :: HNil] { a ⇒ operands match {
      case x :: y :: z :: HNil ⇒ (x.run(a) |@| y.run(a) |@| z.run(a)).map(_ :: _ :: _ :: HNil)
    }
  }

  def eval1[E, A, B, C](operand: Expr[E, A, B], op: Expr[E, B :: HNil, C]) : Expr[E, A, C] =
    Kleisli[λ[α ⇒ Term[E, α]], A, C] { source ⇒ join1(operand :: HNil).run(source) match {
      case i @ Invalid(_) ⇒ i
      case Valid(v :: HNil) ⇒ op.run(v :: HNil)
    }}

  def eval2[E, A, B, C, D](l: Expr[E, A, B], r: Expr[E, A, C], op: Expr[E, B :: C :: HNil, D]) : Expr[E, A, D] =
    Kleisli[λ[α ⇒ Term[E, α]], A, D] { source ⇒ join2(l :: r :: HNil).run(source) match {
      case i @ Invalid(_) ⇒ i
      case Valid(lv :: rv :: HNil) ⇒ op.run(lv :: rv :: HNil)
    }}

  def eval3[E, V, W, X, Y, Z](V: Expr[E, V, W], y: Expr[E, V, X], z: Expr[E, V, Y], op: Expr[E, W :: X :: Y :: HNil, Z]) : Expr[E, V, Z] =
    Kleisli[λ[α ⇒ Term[E, α]], V, Z] { source ⇒ join3(V :: y :: z :: HNil).run(source) match {
      case i @ Invalid(_) ⇒ i
      case Valid(xv :: yv :: zv :: HNil) ⇒ op.run(xv :: yv :: zv :: HNil)
    }}
}
