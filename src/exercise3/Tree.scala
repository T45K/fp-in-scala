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
}
