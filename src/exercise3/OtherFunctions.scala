package exercise3

object OtherFunctions {
  def main(args: Array[String]): Unit = {
    println(List.incrementEach(List(1, 2, 3, 4, 5)))

    println(List.doubleToStringEach(List(1.0, 1.1, 1.2, 1.3, 1.4)))

    println(List.map(List(1, 2, 3, 4, 5))(_.toString))

    println(List.filter(List(1, 2, 3, 4, 5))(_ % 2 == 1))

    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))

    println(List.filterViaFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 1))

    println(List.plusWith(List(1, 2, 3), List(4, 5, 6)))

    println(List.zipWith(List(1, 2, 3), List("a", "b", "c"))((a, b) => a + " " + b))
  }
}
