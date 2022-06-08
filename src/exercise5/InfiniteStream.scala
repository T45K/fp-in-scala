package exercise5

object InfiniteStream {
  def main(args: Array[String]): Unit = {
    println(Stream.constant(5).take(5).toList)

    println(Stream.from(1).take(5).toList)

    println(Stream.fibs().take(5).toList)

    println(Stream.unfold(0)(n => Option((n + 1, n + 1))).take(5).toList)
  }
}
