package exercise5

import exercise5.Stream.{cons, empty, unfold}

import scala.util.chaining.scalaUtilChainingOps

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else t().takeWhile(p)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = this.foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def headOptionViaRightFold: Option[A] = this.foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = this.foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  //  def append[B >: A](b: => B): Stream[B] = this.foldRight(Stream.apply(b))(cons(_, _))

  def append[B >: A](b: => Stream[B]): Stream[B] = this.foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def startsWith[B >: A](s: Stream[B]): Boolean = unfold((this, s)) {
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(h1() == h2(), (t1(), t2()))
  }.forAll(identity)

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some(Cons(h, t), t())
    case _ => None
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(z -> Stream.apply(z)) { (a, b) =>
    // BはFunction型の糖衣構文なので、参照する度に評価される
    lazy val b1 = b
    f(a, b1._1).pipe(r => r -> cons(r, b1._2))
  }._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    // head、tailは一度評価された後は、ただの値になる
    // 直接hdを突っ込むと、Consの先頭を評価する度にhdが評価（実行）される
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(n1: Int = 0, n2: Int = 1): Stream[Int] = cons(n1, fibs(n2, n1 + n2))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => Empty
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  def mapViaUnfold[A, B](a: Stream[A])(f: A => B): Stream[B] = unfold(a) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeViaUnfold[A](a: Stream[A])(n: Int): Stream[A] = unfold((a, n)) {
    case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
    case _ => None
  }

  def zipWithViaUnfold[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] = unfold(a, b) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[A, B](a: Stream[A], b: Stream[B]): Stream[(Option[A], Option[B])] = unfold(a, b) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }
}
