package io.nary.espresso.adapters

import io.nary.espresso.defs
import cats.data._
import cats.syntax.all._

object readers {
  import defs._

  implicit def strIn[E]: In[E, String] = Kleisli[λ[α ⇒ Term[E, α]], Any, String](_.toString.validNel)

  implicit def doubleIn[E](orElse: E): In[E, Double] =
    Kleisli[Lambda[α => Term[E, α]], Any, Double] { v =>
      Either.catchOnly[NumberFormatException](v.toString.toDouble)
        .leftMap(_ => orElse)
        .toValidatedNel
    }

  implicit def intIn[E](orElse: E): In[E, Int] =
    Kleisli[Lambda[α => Term[E, α]], Any, Int] { v =>
      Either.catchOnly[NumberFormatException](v.toString.toInt)
        .leftMap(_ => orElse)
        .toValidatedNel
    }

  def read[E, A, K, S](
                        k: K,
                        orElse: E,
                        f: S => K => In[E, A] => Option[Term[E, A]]
                      )(implicit i: In[E, A]): Expr[E, S, A] =
    Kleisli[Lambda[α => Term[E, α]], S, A] { s =>
      f(s)(k)(i).getOrElse(orElse.invalidNel)
    }
}