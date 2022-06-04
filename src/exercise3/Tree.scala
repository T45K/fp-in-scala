package exercise3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => size(left) + size(right)
    case _ => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(value) => value
    case _ => 0
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => (depth(left) max depth(right)) + 1
    case _ => 0
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(f2: (B, B) => B): B = t match {
    case Branch(left, right) => f2(fold(left)(f)(f2), fold(right)(f)(f2))
    case Leaf(value) => f(value)
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((b1, b2) => (b1 max b2) + 1)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
