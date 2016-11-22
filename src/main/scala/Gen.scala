/**
  * Created by ryan on 8/21/16.
  */
object Gen {
  def genOp(start: Int, end: Int) : Unit = {
    for(i ← start to end) {
      val typeParams = (1 to i).map(x => "A" + x).mkString("[E, ", ", ", ", B]")
      val fDom = (1 to i).map(x => s"Term[E, A$x]").mkString(" :: ") + " :: HNil"
      val ret = (1 to i).map(x => "A" + x).mkString(" :: ") + " :: HNil"
      val args = (1 to i).map(x => "a" + x).mkString(" :: ") + " :: HNil"
      val app = (1 to i).map(x => s"Applicative[λ[α ⇒ Term[E, α]]].pure(a$x)").mkString(" :: ") + " :: HNil"

      val s = s"""def op$i$typeParams(f: ($fDom) ⇒ Term[E, B]) : Expr[E, $ret, B] =
              | Kleisli[λ[α ⇒ Term[E, α]], $ret, B] { case $args ⇒
              |  f($app)
              | }
              """.stripMargin

      println(s)
    }
  }

  def genJoin(start: Int, end: Int) : Unit = {
    for(i ← start to end) {
      val typeParams = (1 to i).map(x => "B" + x).mkString("[E, A, ", ", ", "]")
      val operands = (1 to i).map(x => s"Expr[E, A, B$x]").mkString(" :: ") + " :: HNil"
      val ret = (1 to i).map(x => "B" + x).mkString(" :: ") + " :: HNil"
      val args = (1 to i).map(x => "b" + x).mkString(" :: ") + " :: HNil"
      val app = (1 to i).map(x => s"b$x.run(a)").mkString(" |@| ")
      val appMap = (1 to i).map(_ ⇒ "_").mkString(" :: ") + " :: HNil"

      val s =
        s"""
           |def join$i$typeParams(operands: $operands) : Expr[E, A, $ret] =
           |    Kleisli[λ[α ⇒ Term[E, α]], A, $ret] { a ⇒ operands match {
           |      case $args ⇒ ($app).map($appMap)
           |    }
           |  }
         """.stripMargin

      println(s)
    }
  }

  def genEval(start: Int, end: Int) : Unit = {
    for(i ← start to end) {
      val bs = (1 to i).map(x => "B" + x).mkString(", ")
      val abArgs = (1 to i).map(x ⇒ s"b$x: Expr[E, A, B$x]").mkString(", ")
      val bhlist = (1 to i).map(x ⇒ "B" + x).mkString(" :: ") + " :: HNil"
      val abhlist = (1 to i).map(x ⇒ s"b$x").mkString(" :: ")  + " :: HNil"

      val s =
        s"""
           |def eval$i[E, A, $bs, C]($abArgs, op: Expr[E, $bhlist, C]) : Expr[E, A, C] =
           |    Kleisli[λ[α ⇒ Term[E, α]], A, C] { source ⇒ join$i($abhlist).run(source) match {
           |      case i @ Invalid(_) ⇒ i
           |      case Valid(hlist) ⇒ op.run(hlist)
           |    }}
         """.stripMargin

      println(s)
    }
  }
}
