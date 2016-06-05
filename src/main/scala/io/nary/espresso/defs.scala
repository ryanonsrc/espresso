package io.nary.espresso

import cats.data.{Kleisli, ValidatedNel}

object defs {
  type Term[E, A] = ValidatedNel[E, A]

  type Expr[E, A, B] = Kleisli[({type λ[α] = Term[E, α]})#λ, A, B]
  type In[E, A] = Expr[E, Any, A]
}
