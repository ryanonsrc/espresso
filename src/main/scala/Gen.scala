/**
 * Code generation utilities for espresso.
 * Updated for modern Scala syntax and conventions.
 */
object Gen {
  def genOp(start: Int, end: Int): Unit = {
    for (i <- start to end) {
      val typeParams = (1 to i).map(x => s"A$x").mkString("[E, ", ", ", ", B]")
      val fDom = (1 to i).map(x => s"Term[E, A$x]").mkString(" :: ") + " :: HNil"
      val ret = (1 to i).map(x => s"A$x").mkString(" :: ") + " :: HNil"
      val args = (1 to i).map(x => s"(a$x: A$x)").mkString(" :: ") + " :: HNil"
      val app = (1 to i).map(x => s"Applicative[Lambda[α => Term[E, α]]].pure(a$x)").mkString(" :: ") + " :: HNil"

      val s =
        s"""def op$i$typeParams(f: ($fDom) => Term[E, B]): Expr[E, $ret, B] =
           |  Kleisli.apply[[α] =>> Term[E, α], $ret, B] { case $args =>
           |    f($app)
           |  }""".stripMargin

      println(s)
    }
  }

  def genFuncExpr(start: Int, end: Int): Unit = {
    for (i <- start to end) {
      val typeParams = (1 to i).map(x => s"A$x").mkString("[E, ", ", ", ", B]")
      val hltargs = (1 to i).map(x => s"A$x").mkString(" :: ") + " :: HNil"
      val ctargs = (1 to i).map(x => s"A$x").mkString(", ")
      val hlargs = (1 to i).map(x => s"(a$x: A$x)").mkString(" :: ") + " :: HNil"
      val args = (1 to i).map(x => s"a$x").mkString(", ")

      val s =
        s"""def funcExpr$i$typeParams(f: ($ctargs) => B)(g: String => E): Expr[E, $hltargs, B] =
           |  Kleisli.apply[[α] =>> Term[E, α], $hltargs, B] {
           |    case $hlargs =>
           |      Either.catchNonFatal(f($args)).leftMap(e => g(e.getMessage)).toValidatedNel
           |  }""".stripMargin

      println(s)
    }
  }

  def genJoin(start: Int, end: Int): Unit = {
    for (i <- start to end) {
      val typeParams = (1 to i).map(x => s"B$x").mkString("[E, A, ", ", ", "]")
      val operands = (1 to i).map(x => s"Expr[E, A, B$x]").mkString(" :: ") + " :: HNil"
      val ret = (1 to i).map(x => s"B$x").mkString(" :: ") + " :: HNil"
      val args = (1 to i).map(x => s"(b$x: Expr[E, A, B$x])").mkString(" :: ") + " :: HNil"

      // Replace |@| with modern tuple syntax and mapN
      val runCalls = (1 to i).map(x => s"b$x.run(a)")
      val app = if (i == 1) {
        s"${runCalls.head}.map"
      } else {
        s"(${runCalls.mkString(", ")}).mapN"
      }

      val appMap = (1 to i).map(_ => "_").mkString(" :: ") + " :: HNil"

      val s =
        s"""def join$i$typeParams(operands: $operands): Expr[E, A, $ret] =
           |  Kleisli.apply[[α] =>> Term[E, α], A, $ret] { a => operands match {
           |    case $args => $app($appMap)
           |  }}""".stripMargin

      println(s)
    }
  }

  def genEval(start: Int, end: Int): Unit = {
    for (i <- start to end) {
      val bs = (1 to i).map(x => s"B$x").mkString(", ")
      val abArgs = (1 to i).map(x => s"b$x: Expr[E, A, B$x]").mkString(", ")
      val bhlist = (1 to i).map(x => s"B$x").mkString(" :: ") + " :: HNil"
      val abhlist = (1 to i).map(x => s"b$x").mkString(" :: ") + " :: HNil"

      // For better formatting, split long parameter lists across lines
      val paramList = if (i > 3) {
        s"\n    ${abArgs.split(", ").mkString(",\n    ")},\n    op: Expr[E, $bhlist, C]"
      } else {
        s"$abArgs, op: Expr[E, $bhlist, C]"
      }

      val s =
        s"""def eval$i[E, A, $bs, C]($paramList): Expr[E, A, C] =
           |  Kleisli.apply[[α] =>> Term[E, α], A, C] { source => join$i($abhlist).run(source) match {
           |    case i @ Invalid(_) => i
           |    case Valid(hlist: ($bhlist)) => op.run(hlist)
           |  }}""".stripMargin

      println(s)
    }
  }

  /**
   * Helper method to generate all code at once
   */
  def generateAll(start: Int = 1, end: Int = 10): Unit = {
    println("// Generated Operations")
    genOp(start, end)
    println("\n// Generated Joins")
    genJoin(start, end)
    println("\n// Generated Evaluations")
    genEval(start, end)
    println("\n// Generated Lift FuncExpr")
    genFuncExpr(start, end)
  }

  def main(args: Array[String]): Unit = {
    generateAll()
  }
}