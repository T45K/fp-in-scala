package exercise3

import scala.annotation.tailrec

object FoldLeft {
  def main(args: Array[String]): Unit = {
    @tailrec
    def loop(x: Int, list: List[Int]): List[Int] =
      if (x > 0) loop(x - 1, Cons(x, list))
      else list

    def bigList = loop(1_000_000, Nil)

    println(List.foldLeft(bigList, 0)(_ + _))

    try {
      println(List.foldRight(bigList, 0)(_ + _))
      throw new RuntimeException("StackOverFlowError is expected to be thrown but not")
    } catch {
      case _: StackOverflowError => println("StackOverFlowError occurred")
      case e: Exception => println(e.getMessage)
    }
  }
}
