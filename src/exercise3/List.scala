package exercise3

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => Cons(a, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def loop(l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else
        l match {
          case Nil => Nil
          case Cons(_, tail) => loop(tail, n - 1)
        }
    }

    loop(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def loop(l: List[A]): List[A] =
      l match {
        case Nil => Nil
        case Cons(head, tail) =>
          if (f(head)) loop(tail)
          else l
      }

    loop(l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b: Int) => b + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(as: List[A], acc: B): B = {
      as match {
        case Nil => acc
        case Cons(x, xs) => loop(xs, f(acc, x))
      }
    }

    loop(as, z)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  // asの長さに比例する時間が必要
  def append[A](as: List[A], a: A): List[A] = List.foldRight(as, List(a))(Cons(_, _))

  def append[A](as: List[A], as2: List[A]): List[A] = List.foldRight(as, as2)(Cons(_, _))

  // assのサイズをN、全要素の数をMとするとO(N+M)≒O(M)
  def concat[A](ass: List[List[A]]): List[A] = foldRight(ass, Nil: List[A])(append)

  def incrementEach(ints: List[Int]): List[Int] = foldRight(ints, Nil: List[Int])((int, acc) => Cons(int + 1, acc))

  def doubleToStringEach(ds: List[Double]): List[String] = foldRight(ds, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, acc) => if (f(a)) acc else Cons(a, acc))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))

  // 模範解答
  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) Nil else List(a))

  def plusWith(as1: List[Int], as2: List[Int]): List[Int] =
    if (List.length(as1) != List.length(as2)) Nil
    else List.foldRight(as1, (Nil, as2.pipe(List.reverse)): (List[Int], List[Int]))((a, b) => {
      val (acc, rest) = b
      rest match {
        case Cons(head, tail) => (Cons(a + head, acc), tail) // ここが評価されるのは右側から
      }
    })._1

  // 模範解答
  def plusWith2(as1: List[Int], as2: List[Int]): List[Int] = (as1, as2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, plusWith2(at, bt))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    if (List.length(as) != List.length(bs)) Nil
    else List.foldRight(as, (Nil, bs.pipe(List.reverse)): (List[C], List[B]))((a, b) => {
      val (acc, rest) = b
      rest match {
        case Cons(head, tail) => (Cons(f(a, head), acc), tail)
      }
    })._1

  def zipWith2[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith2(at, bt)(f))
  }

  def startsWith[A](l1: List[A], l2: List[A]): Boolean =
    if (List.length(l2) > List.length(l1)) false
    else if (l2 == Nil) true
    else foldRight(zipWith2(l1, l2)(_ == _), true)(_ && _)

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    if (length(sub) > length(sup)) false
    else if (startsWith(sup, sub)) true
    else sup match {
      case Cons(_, tail) => hasSubsequence(tail, sub)
    }
}
