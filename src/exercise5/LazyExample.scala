package exercise5

object LazyExample {
  def main(args: Array[String]): Unit = {
    // 正格
    // 関数の引数が常に評価される
    def strictMethod(b: Boolean): Boolean = true

    // 引数が評価されるので、Helloが表示される
    strictMethod {
      println("Hello")
      true
    }


    // 非正格、遅延
    // 関数の引数は、関数ないで利用されるタイミングで評価される
    def lazyMethod(b: => Boolean): Boolean = true
    // => はサンク（ラムダ式？）の糖衣構文
    // def lazyMethod(b: () => Boolean): Boolean

    // worldは表示されれない
    lazyMethod {
      println("World")
      true
    }

    // デフォルトでは、評価結果をキャッシュしない
    def maybeTwice(b: Boolean, i: => Int): Int = if (b) i + i else 0

    // Hiが2回表示される
    maybeTwice(true, {
      println("Hi")
      1 + 2
    })

    def maybeOnceStrict(b: Boolean, i: => Int): Int = {
      val j = i // この時点で評価される
      if (b) j + j else 0
    }

    // 関数内でiの計算結果が利用されなくても、iは評価される
    // Hi 2が一度表示される
    maybeOnceStrict(false, {
      println("Hi 2")
      1 + 2
    })

    // lazeでさらに遅延できる
    def maybeOnce(b: Boolean, i: => Int): Int = {
      lazy val j = i // キャッシュ
      if (b) j + j else 0
    }

    // Hi 3は表示されない
    maybeOnce(false, {
      println("Hi 3")
      1 + 2
    })

    // Hi 3が一度だけ表示
    maybeOnce(true, {
      println("Hi 3")
      1 + 2
    })
  }

}
