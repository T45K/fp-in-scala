package exercise4

object Variance {
  def main(args: Array[String]): Unit = {
    def valiance(xs: Seq[Double]): Option[Double] =
      Some(xs).flatMap(xs =>
        if (xs.isEmpty) None
        else Some(xs.sum / xs.size)
      ).map(avg => xs.map(v => Math.pow(v - avg, 2)).sum / xs.size)

    println(valiance(Seq(1, 2, 3, 4, 5)))
  }
}
