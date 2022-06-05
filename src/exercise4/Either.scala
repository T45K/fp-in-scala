package exercise4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
    case Lefts(es) => Lefts(es)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
    case Lefts(es) => Lefts(es)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b match {
      case Right(_) => b
      case Left(_) => b.mergeLeft(e)
    }
    case Right(_) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Left(a), Left(_)) => b.mergeLeft(a)
    case (Left(a), _) => Left(a)
    case (_, Left(b)) => Left(b)
  }

  def map2ViaFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)

  def mergeLeft[EE >: E](e: EE): Lefts[EE] = this match {
    case Left(a) => Lefts(List(e, a))
    case Lefts(es) => Lefts(e :: es)
    case Right(_) => Lefts(List(e))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Lefts[+E](values: List[E]) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(l => hh :: l))
  }

  def traverse[E, A, B](es: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => h.flatMap(f).flatMap(hh => traverse(t)(f).map(l => hh :: l))
  }
}
