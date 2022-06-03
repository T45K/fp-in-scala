package exercise2

import scala.annotation.tailrec

object GenericFunction {

  def main(args: Array[String]): Unit = {
    println(Array(7, 9, 13).isSorted((x: Int, y: Int) => x <= y))
  }

  implicit class ExtendArray[A](as: Array[A]) {
    def isSorted(ordered: (A, A) => Boolean): Boolean = {
      @tailrec
      def loop(n: Int): Boolean = {
        if (n == as.length) true
        else if (ordered(as(n - 1), as(n))) loop(n + 1)
        else false
      }

      loop(1)
    }
  }
}
