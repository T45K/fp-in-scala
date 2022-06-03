package exercise2

object Currying {
  def main(args: Array[String]): Unit = {
    def sum = (a: Int, b: Int) => a + b

    def curriedSum = curry(sum)

    println(sum(1, 2))
    println(curriedSum(1)(2))

    def increment = curriedSum(1)

    println(increment(2))
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = { a: A => b: B => f(a, b) }
}
