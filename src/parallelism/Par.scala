package parallelism

import java.util.concurrent.{ExecutorService, Future}
import scala.concurrent.duration.TimeUnit
import scala.util.chaining.scalaUtilChainingOps

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(eventIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // 再帰を用いた総和計算
  // sum1(l)とsun1(r)を並行で計算できる可能性がある
  //  def sum1(ints: IndexedSeq[Int]): Int =
  //    if (ints.size <= 1) {
  //      ints.headOption getOrElse 0
  //    } else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      sum1(l) + sum1(r)
  //    }

  /* Par#unitの引数を評価するタイミングをどうするか
   1. すぐに評価する場合（正格評価）
      パフォーマンスは良い。sumLとsumRの評価を同時に行えるから
      ただし参照透明ではなくなる。
      Par.get(Par.unit(sum2(l))) + Par.get(Par.unit(sum2(r)))
      が「+の左辺を評価→右辺を評価」という順番で評価されるから
   2. 遅延評価の場合、上の逆
  */
  //  def sum2(ints: IndexedSeq[Int]): Int =
  //    if (ints.size <= 1) ints.headOption getOrElse 0
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      val sumL = Par.unit(sum2(l))
  //      val sumR = Par.unit(sum2(r))
  //      Par.run(sumL) + Par.run(sumR)
  //    }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get()))
  }

  def sum3(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum3(l), sum3(r))(_ + _)
    }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get())

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // map2を正格にできる
  //  def sum4(ints: IndexedSeq[Int]): Par[Int] =
  //    if (ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      Par.map2(Par.fork(sum3(l)), Par.fork(sum3(r)))(_ + _)
  //    }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit[List[A]](Nil))((ph, pt) => map2(ph, pt)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    as.map(asyncF(a => if (f(a)) List(a) else List.empty))
      .pipe(sequence)
      .pipe(map(_)(_.flatten))
  }
}

object Main {
  def main(args: Array[String]): Unit = {

  }
}
