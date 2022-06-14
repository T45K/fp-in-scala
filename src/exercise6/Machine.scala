package exercise6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

// 何もわからん
// Stateの畳み込み？
object Main {
  def main(args: Array[String]): Unit = {
    println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(locked = true, 5, 10))._1)
  }

  //  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  //    for {
  //      _ <- State.traverse(inputs)(i => State.modify(update(i)))
  //      s <- State.get
  //    } yield (s.coins, s.candies)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.traverse(inputs)(i => State.modify(update(i))) // State[Machine, List[Nothing]]
      .flatMap(_ => // List[Nothing] を捨てる
        // flatMapのthisはtraverseした後のStateなので、これにMachineを食わせると、inputs実行後の状態に遷移する
        State.get // State(S => (S, S))
          .map(s => s.coins -> s.candies) // (S, S) を (S, (Int, Int)) に変換する
      ) // State.get.map の結果に置き換わる

  private def update(i: Input)(s: Machine): Machine = (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) =>
      Machine(locked = false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) =>
      Machine(locked = true, candy - 1, coin)
  }
}
