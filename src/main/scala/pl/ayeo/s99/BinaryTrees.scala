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
    if (ord.gt(newValue, value)) Node[A](value, left, right.addValue(newValue))
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
    case n if n % 2 == 1 => for (r <- cBalanced(n / 2, value); l <- cBalanced(n / 2, value))
      yield Node(value, l, r)
    case n if n % 2 == 0 => (for (l <- cBalanced((n - 1) / 2, value); r <- cBalanced((n - 1) / 2 + 1, value))
      yield List(Node[T](value, l, r), Node[T](value, r, l))).flatten
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
      else injector(values.tail, tree.addValue(values.head))
    }

    injector(values, End)
  }

  /**
    * P58 (**) Generate-and-test paradigm.
    * Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given
    * number of nodes.
    * scala> Tree.symmetricBalancedTrees(5, "x")
    * res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
    */
  def symmetricBalancedTrees[A](nodes: Int, value: A): List[Tree[A]] = cBalanced(nodes, value).filter(_.isSymmetric)

  /**
    * P59 (**) Construct height-balanced binary trees.
    * In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and
    * the height of its right subtree are almost equal, which means their difference is not greater than one.
    * Write a method Tree.hbalTrees to construct height-balanced binary trees for a given height with a supplied value
    * for the nodes. The function should generate all solutions.
    * scala> Tree.hbalTrees(3, "x")
    * res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...
    */
  def hbalTrees[T](depth: Int, content: T): List[Tree[T]] = {
    def helper(nodesNumber: Int, toRemove: Int): List[Tree[T]] = {
      if (toRemove == 0) return List()
      else cBalanced(nodesNumber - toRemove, content) ++ helper(nodesNumber, toRemove - 1)
    }

    helper(Math.pow(2, depth).toInt, Math.pow(2, depth - 1).toInt).reverse
  }


  /**
    * P60 (**) Construct height-balanced binary trees with a given number of nodes.
    * Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain? Clearly,
    * MaxN = 2H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive
    * statement and turn it into a function minHbalNodes that takes a height and returns MinN.
    * scala> minHbalNodes(3)
    * res0: Int = 4
    *
    * On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a maxHbalHeight function.
    * scala> maxHbalHeight(4)
    * res1: Int = 3
    *
    * Now, we can attack the main problem: construct all the height-balanced binary trees with a given nuber of nodes.
    * scala> Tree.hbalTreesWithNodes(4, "x")
    * res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
    *
    * Find out how many height-balanced trees exist for N = 15.
    */
  //can not see the difference beteent this and p55. So only last task is solved below
  def getSolutionsNumber(nodesNumber: Int): Int = cBalanced(nodesNumber, 'x).length


  /**
    * P61 (*) Count the leaves of a binary tree.
    * A leaf is a node with no successors. Write a method leafCount to count them.
    * scala> Node('x', Node('x'), End).leafCount
    * res0: Int = 1
    */
  def leafCount[T](tree: Tree[T]): Int = tree match {
    case End => 0
    case Node(_, End, End) => 1
    case Node(_, l, r) => leafCount(l) + leafCount(r)
  }

  /**
    * 61A (*) Collect the leaves of a binary tree in a list.
    * A leaf is a node with no successors. Write a method leafList to collect them in a list.
    * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
    * res0: List[Char] = List(b, d, e)
    */
  def leafList[T](tree: Tree[T]): List[T] = tree match {
    case End => List()
    case Node(value, End, End) => List(value)
    case Node(_, l, r) => leafList(l) ++ leafList(r)
  }

  /**
    * P62 (*) Collect the internal nodes of a binary tree in a list.
    * An internal node of a binary tree has either one or two non-empty successors. Write a method internalList to
    * collect them in a list.
    * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
    * res0: List[Char] = List(a, c)
    */
  def internalList[T](tree: Tree[T]): List[T] = tree match {
    case End => List()
    case Node(_, End, End) => List()
    case Node(value, l, r) => List(value) ++ internalList(l) ++ internalList(r)
  }

  /**
    * P63 (**) Construct a complete binary tree.
    * A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1 contain the maximum number
    * of nodes (i.e 2(i-1) at the level i, note that we start counting the levels from 1 at the root). In level H, which
    * may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in
    * a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the Ends
    * which are not really nodes!) come last.
    *
    * Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
    *
    * We can assign an address number to each node in a complete binary tree by enumerating the nodes in levelorder,
    * starting at the root with number 1. In doing so, we realize that for every node X with address A the following
    * property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, supposed the
    * successors do exist. This fact can be used to elegantly construct a complete binary tree structure. Write a method
    * completeBinaryTree that takes as parameters the number of nodes and the value to put in each node.
    * scala> Tree.completeBinaryTree(6, "x")
    * res0: Node[String] = T(x T(x T(x . .) T(x . .)) T(x T(x . .) .))
    */
  def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = nodes match {
    case 0 =>
      End
    case n if n % 2 == 1 =>
      Node(value, completeBinaryTree(n / 2, value), completeBinaryTree(n / 2, value))
    case n =>
      Node(value, completeBinaryTree((n - 1) / 2 + 1, value), completeBinaryTree((n - 1) / 2, value))
  }
}
