package io.nary.espresso.sample

object Main {
  def main(args: Array[String]): Unit = {
    val map: Map[String, Any] = Map(
      "foo" -> "fig",
      "bar" -> "banana",
      "baz" -> "BLUEberry"
    )

    import Syntax._

//    val expr = "foo".rev & +"bar" & -"baz".rev & ("50".asInt.asStr)
    val expr = "foo".rev & +"bar" & +(-"baz").rev & (str("50").asInt.asStr)

    val result = expr.run(map)

    println(s"Result is : $result")
  }
}