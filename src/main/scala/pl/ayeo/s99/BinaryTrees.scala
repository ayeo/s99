package pl.ayeo.s99

sealed abstract class Tree[+T] {
  /**
    * P56 (**) Symmetric binary trees.
    * Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right
    * subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether
    * a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror
    * image of another. We are only interested in the structure, not in the contents of the nodes.
    *
    * scala> Node('a', Node('b'), Node('c')).isSymmetric
    * res0: Boolean = true
    */
  def isSymmetric: Boolean
  def isMirrorOf[A](tree: Tree[A]): Boolean
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def isSymmetric: Boolean = right.isMirrorOf(left)
  override def isMirrorOf[A](tree: Tree[A]): Boolean = tree match {
    case x: Node[A] => left.isMirrorOf(x.right) && right.isMirrorOf(x.left)
    case _ => false // Node vs End comparison
  }
}
case object End extends Tree[Nothing] {
  override def toString = "."
  override def isSymmetric: Boolean = true
  override def isMirrorOf[A](tree: Tree[A]): Boolean = tree == End
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
  def apply[T](value: T, left: Tree[T]): Node[T] = Node(value, left, End)
}

object Tree {
  /**
    * P55 (**) Construct completely balanced binary trees.
    * In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left
    * subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater
    * than one.
    *
    * Define an object named Tree. Write a function Tree.cBalanced to construct completely balanced binary trees for a
    * given number of nodes. The function should generate all solutions. The function should take as parameters the
    * number of nodes and a single value to put in all of them.
    *
    * scala> Tree.cBalanced(4, "x")
    * res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
    */
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case 0 => List(End)
    case n if n % 2 == 1 => {
      val l1 = cBalanced(n / 2, value)
      for(r <- l1; l <- l1) yield Node(value, l , r)
    }
    case n if n % 2 == 0 => {
      val l1 = cBalanced((n - 1) / 2, value)
      val l2 = cBalanced((n - 1) / 2 + 1, value)
      (for (l <- l1; r <- l2) yield Node(value, l, r)) ++
      (for (l <- l2; r <- l1) yield Node(value, l, r))
    }
  }
}