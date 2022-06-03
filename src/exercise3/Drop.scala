package exercise3

object Drop {
  def main(args: Array[String]): Unit = {
    println(List.drop(List(1, 2, 3, 4, 5), 3))
    println(List.drop(List(1, 2, 3, 4, 5), 10))
  }
}
