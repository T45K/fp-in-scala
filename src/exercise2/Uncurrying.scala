package exercise2

object Uncurrying {
  def main(args: Array[String]): Unit = {
    def sum = (a: Int) => (b: Int) => a + b

    println(sum(1)(2))

    def uncurriedSum = uncurry(sum)

    println(uncurriedSum(1, 2))
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
}
