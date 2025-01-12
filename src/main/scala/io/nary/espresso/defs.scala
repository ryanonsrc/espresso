package io.nary.espresso

import cats.data.{Kleisli, ValidatedNel}

object defs:
  type Term[E, A] = ValidatedNel[E, A]

  // Using type lambda syntax in Scala 3
  type Expr[E, A, B] = Kleisli[[α] =>> Term[E, α], A, B]
  type In[E, A] = Expr[E, Any, A]
