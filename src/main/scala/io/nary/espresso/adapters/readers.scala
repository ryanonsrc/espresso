package io.nary.espresso.adapters

import cats.Functor
import io.nary.espresso.defs
import cats.data._
import cats.std.all._
import cats.syntax.all._

object readers {

  import defs._

  implicit def strIn[E]: In[E, String] = Kleisli[λ[α ⇒ Term[E, α]], Any, String](_.toString.validNel)

  implicit def doubleIn[E](orElse: E): In[E, Double] = Kleisli[λ[α ⇒ Term[E, α]], Any, Double] { v ⇒
    Xor.catchOnly[NumberFormatException](v.toString.toDouble)
      .leftMap(th ⇒ NonEmptyList[E](orElse)).toValidated
  }

  implicit def intIn[E](orElse: E): In[E, Int] = Kleisli[λ[α ⇒ Term[E, α]], Any, Int] { v ⇒
    Xor.catchOnly[NumberFormatException](v.toString.toInt)
      .leftMap(th ⇒ NonEmptyList[E](orElse)).toValidated
  }

  def read[E, A, K, S](k: K, orElse: E, f: S ⇒ K ⇒ In[E, A] ⇒ Option[Term[E, A]])(implicit i: In[E, A]): Expr[E, S, A] =
    Kleisli[λ[α ⇒ Term[E, α]], S, A] { s ⇒
      f(s)(k)(i) getOrElse orElse.invalidNel
    }
}
