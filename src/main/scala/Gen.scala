/**
 * Code generation utilities for espresso.
 * Updated for modern Scala syntax and conventions.
 */
object Gen:
  def genOp(start: Int, end: Int): Unit =
    for (i <- start to end)
      val typeParams = (1 to i).map(x => s"A$x").mkString("[E, ", ", ", ", B]")
      val fDom = (1 to i).map(x => s"Term[E, A$x]").mkString(", ")
      val ret = (1 to i).map(x => s"A$x").mkString(", ")
      val args = (1 to i).map(x => s"a$x").mkString(", ")
      val app = (1 to i).map(x => s"pure a$x").mkString(", ")

      val s =
        s"""def op$i$typeParams(f: ($fDom) => Term[E, B]): Expr[E, ($ret), B] =
           |  Kleisli.apply[[α] =>> Term[E, α], ($ret), B] { case ($args) =>
           |    f($app)
           |  }""".stripMargin

      println(s)


  def genFuncExpr(start: Int, end: Int): Unit =
    for (i <- start to end)
      val typeParams = (1 to i).map(x => s"A$x").mkString("[E, ", ", ", ", B]")
      val targs = (1 to i).map(x => s"A$x").mkString(", ")
      val args = (1 to i).map(x => s"a$x").mkString(", ")

      val s =
        s"""def funcExpr$i$typeParams(f: ($targs) => B)(g: String => E): Expr[E, ($targs), B] =
           |  Kleisli.apply[[α] =>> Term[E, α], ($targs), B] {
           |    case ($args) =>
           |      Either.catchNonFatal(f($args)).leftMap(e => g(e.getMessage)).toValidatedNel
           |  }""".stripMargin

      println(s)


  def genJoin(start: Int, end: Int): Unit =
    for (i <- start to end)
      val typeParams = (1 to i).map(x => s"B$x").mkString("[E, A, ", ", ", "]")
      val operands = (1 to i).map(x => s"Expr[E, A, B$x]").mkString(", ")
      val ret = (1 to i).map(x => s"B$x").mkString(", ")
      val args = (1 to i).map(x => s"b$x").mkString(", ")

      val runCalls = (1 to i).map(x => s"b$x run a").mkString(", ")

      val s =
        s"""def join$i$typeParams(operands: ($operands)): Expr[E, A, ($ret)] =
           |  Kleisli.apply[[α] =>> Term[E, α], A, ($ret)] { a => operands match {
           |    case ($args) => ($runCalls)
           |  }}""".stripMargin

      println(s)

  def genEval(start: Int, end: Int): Unit = {
    for (i <- start to end) {
      val bs = (1 to i).map(x => s"B$x").mkString(", ")
      val paramTypeList = (1 to i).map(x => s"b$x: Expr[E, A, B$x]").mkString(", ")
      val paramList = (1 to i).map(x => s"b$x").mkString(", ")
      
      val s = s"""def eval$i[E, A, $bs, C]($paramTypeList, op: Expr[E, ($bs), C]): Expr[E, A, C] =
           |  Kleisli.apply[[α] =>> Term[E, α], A, C] { source => 
           |    join$i(($paramList)).run(source) match {
           |      case i @ Invalid(_) => i
           |      case Valid(bt) => op.run(bt)
           |  }}""".stripMargin

      println(s)
    }
  }

  /**
   * Helper method to generate all code at once
   */
  def generateAll(start: Int = 1, end: Int = 10): Unit =
    println("// Generated Operations")
    genOp(start, end)
    println("\n// Generated Joins")
    genJoin(start, end)
    println("\n// Generated Evaluations")
    genEval(start, end)
    println("\n// Generated Lift FuncExpr")
    genFuncExpr(start, end)

  def main(args: Array[String]): Unit =
    generateAll()
