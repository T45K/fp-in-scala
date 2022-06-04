package exercise3

object TreeFunctions {
  def main(args: Array[String]): Unit = {
    def tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    println(Tree.size(tree))

    println(Tree.maximum(tree))

    println(Tree.depth(tree))

    println(Tree.map(tree)(_ + 1))
  }
}
