package io.nary.espresso.sample

import scala.language.implicitConversions

object Main:
  def main(args: Array[String]): Unit =
    val map: Map[String, Any] = Map(
      "foo" -> "fig",
      "bar" -> "banana",
      "baz" -> "BLUEberry"
    )

    import Syntax.*
    
    val expr = "foo".rev & ((+"bar") ^=> (s => s.replace("A", "_"))) & "baz".rev & (str("50").asInt.asStr)

    val result = expr.run(map)

    println(s"Result is : $result")