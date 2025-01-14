package io.nary.espresso

import cats.data.{Kleisli, ValidatedNel}

object defs:
  type In[E, A] = Expr[E, Any, A]  // Values Read in from weakly-typed source to Expression
  type Expr[E, A, B] = Kleisli[[α] =>> Term[E, α], A, B]  // String-typed values to Term
  type Term[E, A] = ValidatedNel[E, A] // A Term is simply a validated expression result 

  