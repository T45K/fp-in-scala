package exercise6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val pair = rng.nextInt
    pair match {
      case (a, r) if a < 0 => (~a, r)
      case _ => pair
    }
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (i, r) => (i.toDouble / Int.MaxValue, r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) (Nil, rng)
    else {
      val (i, r) = rng.nextInt
      val (t, r2) = ints(count - 1)(r)
      (i :: t, r2)
    }
}
