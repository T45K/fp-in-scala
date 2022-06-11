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

object Rand {
  // 状態アクション
  // 乱数生成器を受け取って、乱数と次の生成器を返す動作のエイリアス
  type Rand[+A] = RNG => (A, RNG)

  // aをRandという「文脈」に合う形に変換する
  // Some(a)やList(a)とかと同じ？
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      f(a) -> rng2
    }

  def double(s: Rand[Int]): Rand[Double] = map(s)(_.toDouble / Int.MaxValue)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)(_ -> _)

  // foldRight使える
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case h :: t => map2(h, sequence(t))(_ :: _)
    case Nil => unit(Nil)
  }

  def intsViaSequence(counts: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(counts)((rng: RNG) => rng.nextInt))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(RNG.nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) >= mod) unit(mod) else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  // for内包表記
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    flatMap(rb) { b => unit(f(a, b)) }
  }
}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = this.flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- sb
  } yield f(a, b)

  /*
  this.flatMap { a =>
    sb.map { b => f(a,b) }
  }
   */

  // map, flatMapでは文脈（ここではS）を考えず、関心（A）の変換のみに注視する
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = this.run(s)
    f(a).run(s2)
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = ss.foldRight(unit(Nil): State[S, List[A]]) { (s, acc) =>
    s.map2(acc)((h, t) => h :: t)
  }
}
