package io.nary.espresso

import cats.Applicative
import cats.syntax.all.*
import cats.data.Kleisli
import cats.data.Validated.{Invalid, Valid}

import scala.language.postfixOps

object compose:
  import defs.*

  private def opPure[E, A](a: A) : Term[E, A] = Applicative[[α] =>> Term[E, α]].pure(a)
  
  def op1[E, A, B](f: (Term[E, A]) => Term[E, B]): Expr[E, A, B] =
    Kleisli.apply[[α] =>> Term[E, α], A, B] { a => f(opPure(a)) }

  def op2[E, A1, A2, B](f: (Term[E, A1], Term[E, A2]) => Term[E, B]): Expr[E, (A1, A2), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2), B] { case (a1, a2) =>
      f(opPure(a1), opPure(a2))
    }

  def op3[E, A1, A2, A3, B](f: (Term[E, A1], Term[E, A2], Term[E, A3]) => Term[E, B]): Expr[E, (A1, A2, A3), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3), B] { case (a1, a2, a3) =>
      f(opPure(a1), opPure(a2),opPure(a3))
    }

  def op4[E, A1, A2, A3, A4, B](f: (Term[E, A1], Term[E, A2], Term[E, A3], Term[E, A4]) => Term[E, B]): Expr[E, (A1, A2, A3, A4), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4), B] { case (a1, a2, a3, a4) =>
      f(opPure(a1), opPure(a2), opPure(a3), opPure(a4))
    }

  def op5[E, A1, A2, A3, A4, A5, B](f: (Term[E, A1], Term[E, A2], Term[E, A3], Term[E, A4], Term[E, A5]) => Term[E, B]): Expr[E, (A1, A2, A3, A4, A5), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5), B] { case (a1, a2, a3, a4, a5) =>
      f(opPure(a1), opPure(a2), opPure(a3), opPure(a4), opPure(a5))
    }

  def op6[E, A1, A2, A3, A4, A5, A6, B](f: (Term[E, A1], Term[E, A2], Term[E, A3], Term[E, A4], Term[E, A5], Term[E, A6]) => Term[E, B]): Expr[E, (A1, A2, A3, A4, A5, A6), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6), B] { case (a1, a2, a3, a4, a5, a6) =>
      f(opPure(a1), opPure(a2), opPure(a3), opPure(a4), opPure(a5), opPure(a6))
    }

  def op7[E, A1, A2, A3, A4, A5, A6, A7, B](f: (Term[E, A1], Term[E, A2], Term[E, A3], Term[E, A4], Term[E, A5], Term[E, A6], Term[E, A7]) => Term[E, B]): Expr[E, (A1, A2, A3, A4, A5, A6, A7), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7), B] { case (a1, a2, a3, a4, a5, a6, a7) =>
      f(opPure(a1), opPure(a2), opPure(a3), opPure(a4), opPure(a5), opPure(a6), opPure(a7))
    }

  def op8[E, A1, A2, A3, A4, A5, A6, A7, A8, B](f: (Term[E, A1], Term[E, A2], Term[E, A3], Term[E, A4], Term[E, A5], Term[E, A6], Term[E, A7], Term[E, A8]) => Term[E, B]): Expr[E, (A1, A2, A3, A4, A5, A6, A7, A8), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7, A8), B] { case (a1, a2, a3, a4, a5, a6, a7, a8) =>
      f(opPure(a1), opPure(a2), opPure(a3), opPure(a4), opPure(a5), opPure(a6), opPure(a7), opPure(a8))
    }

  def op9[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, B](f: (Term[E, A1], Term[E, A2], Term[E, A3], Term[E, A4], Term[E, A5], Term[E, A6], Term[E, A7], Term[E, A8], Term[E, A9]) => Term[E, B]): Expr[E, (A1, A2, A3, A4, A5, A6, A7, A8, A9), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7, A8, A9), B] { case (a1, a2, a3, a4, a5, a6, a7, a8, a9) =>
      f(opPure(a1), opPure(a2), opPure(a3), opPure(a4), opPure(a5), opPure(a6), opPure(a7), opPure(a8), opPure(a9))
    }

  def op10[E, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](f: (Term[E, A1], Term[E, A2], Term[E, A3], Term[E, A4], Term[E, A5], Term[E, A6], Term[E, A7], Term[E, A8], Term[E, A9], Term[E, A10]) => Term[E, B]): Expr[E, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), B] =
    Kleisli.apply[[α] =>> Term[E, α], (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), B] { case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =>
      f(opPure(a1), opPure(a2), opPure(a3), opPure(a4), opPure(a5), opPure(a6), opPure(a7), opPure(a8), opPure(a9), opPure(a10))
    }

  def join1[E, A, B](operands: Expr[E, A, B]): Expr[E, A, B] =
    Kleisli.apply[[α] =>> Term[E, α], A, B] { a => operands match { case b => b run a } }

  def join2[E, A, B1, B2](operands: (Expr[E, A, B1], Expr[E, A, B2])): Expr[E, A, (B1, B2)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2)] { a =>
      operands match {
        case (b1, b2) => (b1 run a, b2 run a).mapN((_, _))
      }
    }

  def join3[E, A, B1, B2, B3](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3])): Expr[E, A, (B1, B2, B3)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3)] { a =>
      operands match {
        case (b1, b2, b3) => (b1 run a, b2 run a, b3 run a).mapN((_, _, _))
      }
    }

  def join4[E, A, B1, B2, B3, B4](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3], Expr[E, A, B4])): Expr[E, A, (B1, B2, B3, B4)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3, B4)] { a =>
      operands match {
        case (b1, b2, b3, b4) => (b1 run a, b2 run a, b3 run a, b4 run a).mapN((_, _, _, _))
      }
    }

  def join5[E, A, B1, B2, B3, B4, B5](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3], Expr[E, A, B4], Expr[E, A, B5])): Expr[E, A, (B1, B2, B3, B4, B5)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3, B4, B5)] { a =>
      operands match {
        case (b1, b2, b3, b4, b5) => (b1 run a, b2 run a, b3 run a, b4 run a, b5 run a).mapN((_, _, _, _, _))
      }
    }

  def join6[E, A, B1, B2, B3, B4, B5, B6](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3], Expr[E, A, B4], Expr[E, A, B5], Expr[E, A, B6])): Expr[E, A, (B1, B2, B3, B4, B5, B6)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3, B4, B5, B6)] { a =>
      operands match {
        case (b1, b2, b3, b4, b5, b6) => (b1 run a, b2 run a, b3 run a, b4 run a, b5 run a, b6 run a).mapN((_, _, _, _, _, _))
      }
    }

  def join7[E, A, B1, B2, B3, B4, B5, B6, B7](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3], Expr[E, A, B4], Expr[E, A, B5], Expr[E, A, B6], Expr[E, A, B7])): Expr[E, A, (B1, B2, B3, B4, B5, B6, B7)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3, B4, B5, B6, B7)] { a =>
      operands match {
        case (b1, b2, b3, b4, b5, b6, b7) => (b1 run a, b2 run a, b3 run a, b4 run a, b5 run a, b6 run a, b7 run a).mapN((_, _, _, _, _, _, _))
      }
    }

  def join8[E, A, B1, B2, B3, B4, B5, B6, B7, B8](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3], Expr[E, A, B4], Expr[E, A, B5], Expr[E, A, B6], Expr[E, A, B7], Expr[E, A, B8])): Expr[E, A, (B1, B2, B3, B4, B5, B6, B7, B8)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3, B4, B5, B6, B7, B8)] { a =>
      operands match {
        case (b1, b2, b3, b4, b5, b6, b7, b8) => (b1 run a, b2 run a, b3 run a, b4 run a, b5 run a, b6 run a, b7 run a, b8 run a).mapN((_, _, _, _, _, _, _, _))
      }
    }

  def join9[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3], Expr[E, A, B4], Expr[E, A, B5], Expr[E, A, B6], Expr[E, A, B7], Expr[E, A, B8], Expr[E, A, B9])): Expr[E, A, (B1, B2, B3, B4, B5, B6, B7, B8, B9)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3, B4, B5, B6, B7, B8, B9)] { a =>
      operands match {
        case (b1, b2, b3, b4, b5, b6, b7, b8, b9) => (b1 run a, b2 run a, b3 run a, b4 run a, b5 run a, b6 run a, b7 run a, b8 run a, b9 run a).mapN((_, _, _, _, _, _, _, _, _))
      }
    }

  def join10[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](operands: (Expr[E, A, B1], Expr[E, A, B2], Expr[E, A, B3], Expr[E, A, B4], Expr[E, A, B5], Expr[E, A, B6], Expr[E, A, B7], Expr[E, A, B8], Expr[E, A, B9], Expr[E, A, B10])): Expr[E, A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10)] =
    Kleisli.apply[[α] =>> Term[E, α], A, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10)] { a =>
      operands match {
        case (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) => (b1 run a, b2 run a, b3 run a, b4 run a, b5 run a, b6 run a, b7 run a, b8 run a, b9 run a, b10 run a).mapN((_, _, _, _, _, _, _, _, _, _))
      }
    }

  def eval1[E, A, B, C](b: Expr[E, A, B], op: Expr[E, B, C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join1(b).run(source) match {
        case i@Invalid(_) => i
        case Valid(b) => op.run(b)
      }
    }
  
  def eval2[E, A, B1, B2, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], op: Expr[E, (B1, B2), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join2((b1, b2)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval3[E, A, B1, B2, B3, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], op: Expr[E, (B1, B2, B3), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join3((b1, b2, b3)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval4[E, A, B1, B2, B3, B4, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], b4: Expr[E, A, B4], op: Expr[E, (B1, B2, B3, B4), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join4((b1, b2, b3, b4)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval5[E, A, B1, B2, B3, B4, B5, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], b4: Expr[E, A, B4], b5: Expr[E, A, B5], op: Expr[E, (B1, B2, B3, B4, B5), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join5((b1, b2, b3, b4, b5)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval6[E, A, B1, B2, B3, B4, B5, B6, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], b4: Expr[E, A, B4], b5: Expr[E, A, B5], b6: Expr[E, A, B6], op: Expr[E, (B1, B2, B3, B4, B5, B6), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join6((b1, b2, b3, b4, b5, b6)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval7[E, A, B1, B2, B3, B4, B5, B6, B7, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], b4: Expr[E, A, B4], b5: Expr[E, A, B5], b6: Expr[E, A, B6], b7: Expr[E, A, B7], op: Expr[E, (B1, B2, B3, B4, B5, B6, B7), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join7((b1, b2, b3, b4, b5, b6, b7)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval8[E, A, B1, B2, B3, B4, B5, B6, B7, B8, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], b4: Expr[E, A, B4], b5: Expr[E, A, B5], b6: Expr[E, A, B6], b7: Expr[E, A, B7], b8: Expr[E, A, B8], op: Expr[E, (B1, B2, B3, B4, B5, B6, B7, B8), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join8((b1, b2, b3, b4, b5, b6, b7, b8)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval9[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], b4: Expr[E, A, B4], b5: Expr[E, A, B5], b6: Expr[E, A, B6], b7: Expr[E, A, B7], b8: Expr[E, A, B8], b9: Expr[E, A, B9], op: Expr[E, (B1, B2, B3, B4, B5, B6, B7, B8, B9), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join9((b1, b2, b3, b4, b5, b6, b7, b8, b9)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }

  def eval10[E, A, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, C](b1: Expr[E, A, B1], b2: Expr[E, A, B2], b3: Expr[E, A, B3], b4: Expr[E, A, B4], b5: Expr[E, A, B5], b6: Expr[E, A, B6], b7: Expr[E, A, B7], b8: Expr[E, A, B8], b9: Expr[E, A, B9], b10: Expr[E, A, B10], op: Expr[E, (B1, B2, B3, B4, B5, B6, B7, B8, B9, B10), C]): Expr[E, A, C] =
    Kleisli.apply[[α] =>> Term[E, α], A, C] { source =>
      join10((b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)).run(source) match {
        case i@Invalid(_) => i
        case Valid(bt) => op.run(bt)
      }
    }


  
