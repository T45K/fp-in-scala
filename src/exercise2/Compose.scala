package exercise2

import scala.util.chaining.scalaUtilChainingOps

object Compose {
  def main(args: Array[String]): Unit = {
    def increment = (a: Int) => a + 1

    def decrement = (a: Int) => a - 1

    def neutralize = compose(increment, decrement)

    println(neutralize(1))
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => a.pipe(g).pipe(f)
  }
}
