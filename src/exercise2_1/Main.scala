package exercise2_1

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println(fib(5))
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev1: Int, prev2: Int): Int =
      if (n <= 0) prev1
      else loop(n - 1, prev2, prev1 + prev2)

    loop(n, 0, 1)
  }
}
