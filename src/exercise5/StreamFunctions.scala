package exercise5

object StreamFunctions {
  def main(args: Array[String]): Unit = {
    def generator = () => Stream.apply(1, 2, 3, 4, 5)

    println(generator().toList)

    println(generator().take(3).toList)

    println(generator().take(0).toList)

    println(generator().drop(3).toList)

    println(generator().drop(5).toList)

    println(generator().takeWhile(_ % 2 == 0).toList)
  }
}
