package io.nary.espresso

import cats.Applicative
import cats.syntax.all._
import cats.data.{Kleisli, NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import shapeless.{::, HNil}

object compose {
  import defs._

  def op1[E, A, B](f: (Term[E, A] :: HNil) => Term[E, B]): Expr[E, A :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A :: HNil, B] { case op :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(op) :: HNil)
    }

  def op2[E, A1, A2, B](f: (Term[E, A1] :: Term[E, A2] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: HNil, B] { case a1 :: a2 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: HNil)
    }

  def op3[E, A1, A2, A3, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: HNil, B] { case a1 :: a2 :: a3 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: HNil)
    }

  def op4[E, A1, A2, A3, A4, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: Term[E, A4] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: A4 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: A4 :: HNil, B] { case a1 :: a2 :: a3 :: a4 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: Applicative[Lambda[α => Term[E, α]]].pure(a4) :: HNil)
    }

  def op5[E, A1, A2, A3, A4, A5, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: Term[E, A4] :: Term[E, A5] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: A4 :: A5 :: HNil, B] { case a1 :: a2 :: a3 :: a4 :: a5 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: Applicative[Lambda[α => Term[E, α]]].pure(a4) :: Applicative[Lambda[α => Term[E, α]]].pure(a5) :: HNil)
    }

  def op6[E, A1, A2, A3, A4, A5, A6, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: Term[E, A4] :: Term[E, A5] :: Term[E, A6] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil, B] { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: Applicative[Lambda[α => Term[E, α]]].pure(a4) :: Applicative[Lambda[α => Term[E, α]]].pure(a5) :: Applicative[Lambda[α => Term[E, α]]].pure(a6) :: HNil)
    }

  def op7[E, A1, A2, A3, A4, A5, A6, A7, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: Term[E, A4] :: Term[E, A5] :: Term[E, A6] :: Term[E, A7] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: HNil, B] { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: Applicative[Lambda[α => Term[E, α]]].pure(a4) :: Applicative[Lambda[α => Term[E, α]]].pure(a5) :: Applicative[Lambda[α => Term[E, α]]].pure(a6) :: Applicative[Lambda[α => Term[E, α]]].pure(a7) :: HNil)
    }

  def op8[E, A1, A2, A3, A4, A5, A6, A7, A8, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: Term[E, A4] :: Term[E, A5] :: Term[E, A6] :: Term[E, A7] :: Term[E, A8] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: HNil, B] { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: Applicative[Lambda[α => Term[E, α]]].pure(a4) :: Applicative[Lambda[α => Term[E, α]]].pure(a5) :: Applicative[Lambda[α => Term[E, α]]].pure(a6) :: Applicative[Lambda[α => Term[E, α]]].pure(a7) :: Applicative[Lambda[α => Term[E, α]]].pure(a8) :: HNil)
    }

  def op9[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: Term[E, A4] :: Term[E, A5] :: Term[E, A6] :: Term[E, A7] :: Term[E, A8] :: Term[E, A9] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: HNil, B] { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: Applicative[Lambda[α => Term[E, α]]].pure(a4) :: Applicative[Lambda[α => Term[E, α]]].pure(a5) :: Applicative[Lambda[α => Term[E, α]]].pure(a6) :: Applicative[Lambda[α => Term[E, α]]].pure(a7) :: Applicative[Lambda[α => Term[E, α]]].pure(a8) :: Applicative[Lambda[α => Term[E, α]]].pure(a9) :: HNil)
    }

  def op10[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](f: (Term[E, A1] :: Term[E, A2] :: Term[E, A3] :: Term[E, A4] :: Term[E, A5] :: Term[E, A6] :: Term[E, A7] :: Term[E, A8] :: Term[E, A9] :: Term[E, A10] :: HNil) => Term[E, B]): Expr[E, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: HNil, B] =
    Kleisli[Lambda[α => Term[E, α]], A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: A7 :: A8 :: A9 :: A10 :: HNil, B] { case a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: a7 :: a8 :: a9 :: a10 :: HNil =>
      f(Applicative[Lambda[α => Term[E, α]]].pure(a1) :: Applicative[Lambda[α => Term[E, α]]].pure(a2) :: Applicative[Lambda[α => Term[E, α]]].pure(a3) :: Applicative[Lambda[α => Term[E, α]]].pure(a4) :: Applicative[Lambda[α => Term[E, α]]].pure(a5) :: Applicative[Lambda[α => Term[E, α]]].pure(a6) :: Applicative[Lambda[α => Term[E, α]]].pure(a7) :: Applicative[Lambda[α => Term[E, α]]].pure(a8) :: Applicative[Lambda[α => Term[E, α]]].pure(a9) :: Applicative[Lambda[α => Term[E, α]]].pure(a10) :: HNil)
    }

  def join1[E, A, B](operand: Expr[E, A, B] :: HNil): Expr[E, A, B :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B :: HNil] { a => operand match {
      case b :: HNil => b.run(a).map(_ :: HNil)
    }}

  def join2[E, A, B1, B2](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: HNil): Expr[E, A, B1 :: B2 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: HNil] { a => operands match {
      case b1 :: b2 :: HNil => (b1.run(a), b2.run(a)).mapN(_ :: _ :: HNil)
    }}

  def join3[E, A, B1, B2, B3](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a)).mapN(_ :: _ :: _ :: HNil)
    }}

  def join4[E, A, B1, B2, B3, B4](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: Expr[E, A, B4] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: B4 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: B4 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: b4 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a), b4.run(a)).mapN(_ :: _ :: _ :: _ :: HNil)
    }}

  def join5[E, A, B1, B2, B3, B4, B5](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: Expr[E, A, B4] :: Expr[E, A, B5] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: B4 :: B5 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: B4 :: B5 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a), b4.run(a), b5.run(a)).mapN(_ :: _ :: _ :: _ :: _ :: HNil)
    }}

  def join6[E, A, B1, B2, B3, B4, B5, B6](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: Expr[E, A, B4] :: Expr[E, A, B5] :: Expr[E, A, B6] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a), b4.run(a), b5.run(a), b6.run(a)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: HNil)
    }}

  def join7[E, A, B1, B2, B3, B4, B5, B6, B7](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: Expr[E, A, B4] :: Expr[E, A, B5] :: Expr[E, A, B6] :: Expr[E, A, B7] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a), b4.run(a), b5.run(a), b6.run(a), b7.run(a)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
    }}

  def join8[E, A, B1, B2, B3, B4, B5, B6, B7, B8](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: Expr[E, A, B4] :: Expr[E, A, B5] :: Expr[E, A, B6] :: Expr[E, A, B7] :: Expr[E, A, B8] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a), b4.run(a), b5.run(a), b6.run(a), b7.run(a), b8.run(a)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
    }}

  def join9[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: Expr[E, A, B4] :: Expr[E, A, B5] :: Expr[E, A, B6] :: Expr[E, A, B7] :: Expr[E, A, B8] :: Expr[E, A, B9] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: B9 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: B9 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a), b4.run(a), b5.run(a), b6.run(a), b7.run(a), b8.run(a), b9.run(a)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
    }}

  def join10[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](operands: Expr[E, A, B1] :: Expr[E, A, B2] :: Expr[E, A, B3] :: Expr[E, A, B4] :: Expr[E, A, B5] :: Expr[E, A, B6] :: Expr[E, A, B7] :: Expr[E, A, B8] :: Expr[E, A, B9] :: Expr[E, A, B10] :: HNil): Expr[E, A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: B9 :: B10 :: HNil] =
    Kleisli[Lambda[α => Term[E, α]], A, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: B9 :: B10 :: HNil] { a => operands match {
      case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 :: b10 :: HNil =>
        (b1.run(a), b2.run(a), b3.run(a), b4.run(a), b5.run(a), b6.run(a), b7.run(a), b8.run(a), b9.run(a), b10.run(a)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
    }}

  def eval1[E, A, B, C](b: Expr[E, A, B], op: Expr[E, B :: HNil, C]): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join1(b :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval2[E, A, B1, B2, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], op: Expr[E, B1 :: B2 :: HNil, C]): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join2(b1 :: b2 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval3[E, A, B1, B2, B3, C](
                                  b1: Expr[E, A, B1],
                                  b2: Expr[E, A, B2],
                                  b3: Expr[E, A, B3],
                                  op: Expr[E, B1 :: B2 :: B3 :: HNil, C]
                                ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join3(b1 :: b2 :: b3 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval4[E, A, B1, B2, B3, B4, C](
                                      b1: Expr[E, A, B1],
                                      b2: Expr[E, A, B2],
                                      b3: Expr[E, A, B3],
                                      b4: Expr[E, A, B4],
                                      op: Expr[E, B1 :: B2 :: B3 :: B4 :: HNil, C]
                                    ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join4(b1 :: b2 :: b3 :: b4 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval5[E, A, B1, B2, B3, B4, B5, C](
                                          b1: Expr[E, A, B1],
                                          b2: Expr[E, A, B2],
                                          b3: Expr[E, A, B3],
                                          b4: Expr[E, A, B4],
                                          b5: Expr[E, A, B5],
                                          op: Expr[E, B1 :: B2 :: B3 :: B4 :: B5 :: HNil, C]
                                        ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join5(b1 :: b2 :: b3 :: b4 :: b5 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval6[E, A, B1, B2, B3, B4, B5, B6, C](
                                              b1: Expr[E, A, B1],
                                              b2: Expr[E, A, B2],
                                              b3: Expr[E, A, B3],
                                              b4: Expr[E, A, B4],
                                              b5: Expr[E, A, B5],
                                              b6: Expr[E, A, B6],
                                              op: Expr[E, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: HNil, C]
                                            ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join6(b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval7[E, A, B1, B2, B3, B4, B5, B6, B7, C](
                                                  b1: Expr[E, A, B1],
                                                  b2: Expr[E, A, B2],
                                                  b3: Expr[E, A, B3],
                                                  b4: Expr[E, A, B4],
                                                  b5: Expr[E, A, B5],
                                                  b6: Expr[E, A, B6],
                                                  b7: Expr[E, A, B7],
                                                  op: Expr[E, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: HNil, C]
                                                ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join7(b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval8[E, A, B1, B2, B3, B4, B5, B6, B7, B8, C](
                                                      b1: Expr[E, A, B1],
                                                      b2: Expr[E, A, B2],
                                                      b3: Expr[E, A, B3],
                                                      b4: Expr[E, A, B4],
                                                      b5: Expr[E, A, B5],
                                                      b6: Expr[E, A, B6],
                                                      b7: Expr[E, A, B7],
                                                      b8: Expr[E, A, B8],
                                                      op: Expr[E, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: HNil, C]
                                                    ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join8(b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval9[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9, C](
                                                          b1: Expr[E, A, B1],
                                                          b2: Expr[E, A, B2],
                                                          b3: Expr[E, A, B3],
                                                          b4: Expr[E, A, B4],
                                                          b5: Expr[E, A, B5],
                                                          b6: Expr[E, A, B6],
                                                          b7: Expr[E, A, B7],
                                                          b8: Expr[E, A, B8],
                                                          b9: Expr[E, A, B9],
                                                          op: Expr[E, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: B9 :: HNil, C]
                                                        ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join9(b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}

  def eval10[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, C](
                                                                b1: Expr[E, A, B1],
                                                                b2: Expr[E, A, B2],
                                                                b3: Expr[E, A, B3],
                                                                b4: Expr[E, A, B4],
                                                                b5: Expr[E, A, B5],
                                                                b6: Expr[E, A, B6],
                                                                b7: Expr[E, A, B7],
                                                                b8: Expr[E, A, B8],
                                                                b9: Expr[E, A, B9],
                                                                b10: Expr[E, A, B10],
                                                                op: Expr[E, B1 :: B2 :: B3 :: B4 :: B5 :: B6 :: B7 :: B8 :: B9 :: B10 :: HNil, C]
                                                              ): Expr[E, A, C] =
    Kleisli[Lambda[α => Term[E, α]], A, C] { source => join10(b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 :: b10 :: HNil).run(source) match {
      case i @ Invalid(_) => i
      case Valid(hlist) => op.run(hlist)
    }}
}
