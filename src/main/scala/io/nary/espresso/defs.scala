package io.nary.espresso

import cats.data.{Kleisli, ValidatedNel}

object defs {
  type Term[E, A] = ValidatedNel[E, A]

  type Expr[E, A, B] = Kleisli[λ[α ⇒ Term[E, α]], A, B]
  type In[E, A] = Expr[E, Any, A]
}
