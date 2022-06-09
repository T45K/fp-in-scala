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

    println(generator().foldRight(0)(_ + _))

    println(generator().exists(_ == 1))

    println(generator().forAll(_ <= 5))

    println(generator().headOptionViaRightFold)

    println(generator().map(_ + 1).toList)

    println(generator().append(Stream.apply(6, 7, 8)).toList)

    println(Stream.apply(Stream.apply(1, 2, 3), Stream.apply(4, 5, 6)).flatMap(identity).toList)

    println(Stream.apply(1, 2, 3).startsWith(Stream.apply(1, 2)))

    println(generator().tails.map(_.toList).toList)
  }
}
