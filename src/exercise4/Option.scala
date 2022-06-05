package exercise4

import scala.annotation.tailrec

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    if (this.map(f).getOrElse(false)) this
    else None
}

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(v => b.map(f(v, _)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def loop(a: List[Option[A]], acc: List[A]): Option[List[A]] = a match {
      case Nil => Some(acc)
      case Some(a) :: tail => loop(tail, acc.appended(a))
      case None :: _ => None
    }

    loop(a, Nil)
  }

  def sequence_answer[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case h :: tail => h.flatMap(hh => sequence_answer(tail).map(l => hh :: l)) // NoneでもflatMapは使える
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(l => hh :: l))
  }
}
