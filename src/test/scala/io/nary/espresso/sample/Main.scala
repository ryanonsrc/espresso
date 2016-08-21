package io.nary.espresso.sample

/**
  * Created by ryan on 6/4/16.
  */
object Main {
  def main(args: Array[String]) {
    val map : Map[String, Any] =
      Map("foo" → "fig", "bar" → "banana", "baz" → "blueberry")

    import Syntax._

    val expr = "foo".rev & "bar" & "baz".rev

    val result = expr.run(map)

    println(s"Result is : $result")
  }
}
