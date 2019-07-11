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
  def addValue[A >: T](newValue: A)(implicit ord: Ordering[A]): Tree[A]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def isSymmetric: Boolean = right.isMirrorOf(left)
  override def isMirrorOf[A](tree: Tree[A]): Boolean = tree match {
    case x: Node[A] => left.isMirrorOf(x.right) && right.isMirrorOf(x.left)
    case _ => false // Node vs End comparison
  }
  override def addValue[A >: T](newValue: A)(implicit ord: Ordering[A]): Tree[A] = {
    if (ord.gt(newValue,value)) Node[A](value, left, right.addValue(newValue))
    else Node[A](value, left.addValue(newValue), right)
  }

}

case object End extends Tree[Nothing] {
  override def toString = "."
  override def isSymmetric: Boolean = true
  override def isMirrorOf[A](tree: Tree[A]): Boolean = tree == End
  override def addValue[A](value: A)(implicit ord: Ordering[A]): Tree[A] = Node[A](value)
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

  /**
    * P57 (**) Binary search trees (dictionaries).
    * Write a function to add an element to a binary search tree.
    * scala> End.addValue(2)
    * res0: Node[Int] = T(2 . .)
    *
    * scala> res0.addValue(3)
    * res1: Node[Int] = T(2 . T(3 . .))
    *
    * scala> res1.addValue(0)
    * res2: Node[Int] = T(2 T(0 . .) T(3 . .))
    * Hint: The abstract definition of addValue in Tree should be def addValue[U >: T <% Ordered[U]](x: U): Tree[U].
    * The >: T is because addValue's parameters need to be contravariant in T. (Conceptually, we're adding nodes above
    * existing nodes. In order for the subnodes to be of type T or any subtype, the upper nodes must be of type T or
    * any supertype.) The <% Ordered[U] allows us to use the < operator on the values in the tree.
    *
    * Use that function to construct a binary tree from a list of integers.
    *
    * scala> Tree.fromList(List(3, 2, 5, 7, 1))
    * res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
    * Finally, use that function to test your solution to P56.
    *
    * scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
    * res4: Boolean = true
    *
    * scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
    * res5: Boolean = false
    */
  def fromList[T](values: List[T])(implicit ord: Ordering[T]): Tree[T] = {
    def injector(values: List[T], tree: Tree[T]): Tree[T] = {
      if (values.isEmpty) tree
      else  injector(values.tail, tree.addValue(values.head))
    }
    injector(values, End)
  }
}