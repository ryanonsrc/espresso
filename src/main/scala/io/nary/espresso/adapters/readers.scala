package io.nary.espresso.adapters

import io.nary.espresso.defs
import cats.data.*
import cats.syntax.all.*

object readers:
  import defs.*

  given [E]: In[E, String] =
    Kleisli.apply[[α] =>> Term[E, α], Any, String](_.toString.validNel)

  given [E](using orElse: E): In[E, Double] =
    Kleisli.apply[[α] =>> Term[E, α], Any, Double] { v =>
      Either.catchOnly[NumberFormatException](v.toString.toDouble)
        .leftMap(_ => orElse)
        .toValidatedNel
    }

  given [E](using orElse: E): In[E, Int] =
    Kleisli.apply[[α] =>> Term[E, α], Any, Int] { v =>
      Either.catchOnly[NumberFormatException](v.toString.toInt)
        .leftMap(_ => orElse)
        .toValidatedNel
    }

  def read[E, A, K, S](
                        k: K,
                        orElse: E,
                        f: S => K => In[E, A] => Option[Term[E, A]]
                      )(using i: In[E, A]): Expr[E, S, A] =
    Kleisli.apply[[α] =>> Term[E, α], S, A] { s =>
      f(s)(k)(i).getOrElse(orElse.invalidNel)
    }