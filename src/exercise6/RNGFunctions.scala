package exercise6

object RNGFunctions {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(200)

    println(RNG.nonNegativeInt(rng))

    println(RNG.double(rng))

    println(RNG.ints(10)(rng))
  }
}
