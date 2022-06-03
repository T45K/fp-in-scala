package exercise3

object DropWhile {
  def main(args: Array[String]): Unit = {
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 3))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 10))
  }
}
