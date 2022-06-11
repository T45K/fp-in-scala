package exercise6

object RNGFunctions {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(200)

    println(RNG.nonNegativeInt(rng))

    println(RNG.double(rng))

    println(RNG.ints(10)(rng))

    println(Rand.intsViaSequence(10)(rng))

    println(Rand.sequence(List.fill(10)(Rand.nonNegativeLessThan(10)))(rng))
  }
}
