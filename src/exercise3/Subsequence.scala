package exercise3

object Subsequence {
  def main(args: Array[String]): Unit = {
    println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3)))

    println(List.hasSubsequence(List(1, 2, 3, 4, 5), List(3, 5)))
  }
}
