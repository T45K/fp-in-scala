package exercise4

object EitherBasicFunctions {
  def main(args: Array[String]): Unit = {
    def r = Right("Hello")

    println(r.map(_ + ", world"))
    println(r.flatMap(_ => Left("failed!")))
    println(r.orElse(Left("failed!")))
    println(r.map2(Right(", world"))(_ + _))
    println(r.map2(Left("failed!"))(_ + _))

    def l = Left("failed!")

    println(l.map("success?"))
  }
}
