package exercise3

import exercise3.List.{foldLeft, foldRight}

object Reverse {
  def main(args: Array[String]): Unit = {
    def reverseOnFoldLeft[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

    println(reverseOnFoldLeft(List(1, 2, 3, 4, 5)))
    // Consは第一引数を先頭に挿入する
    // foldLeftは左側から畳み込むので後の要素ほど先頭に挿入される

    def reverseOnFoldRight[A](as: List[A]): List[A] = foldRight(as, Nil: List[A])((a, acc) => Cons(a, acc))

    println(reverseOnFoldRight(List(1, 2, 3, 4, 5)))
    // これは正しく動作しない
    // foldRightは右側から畳み込むため
  }
}
