package pl.ayeo.s99

import org.scalatest.FunSuite

class BinaryTreesTest extends FunSuite {
  test("Completely balanced tree with 2 nodes") {
    val expected = "List(T('x . T('x . .)), T('x T('x . .) .))"
    val result = Tree.cBalanced(2, 'x).toString()
    assert(expected == result)
  }

  test("Completely balanced tree with 3 nodes") {
    val expected = "List(T('x T('x . .) T('x . .)))"
    val result = Tree.cBalanced(3, 'x).toString()
    assert(expected == result)
  }

  test("Completely balanced tree with 4 nodes") {
    val expected = "List(T('x T('x . .) T('x . T('x . .))), T('x T('x . T('x . .)) T('x . .)), T('x T('x . .) T('x T('x . .) .)), T('x T('x T('x . .) .) T('x . .)))"
    val result = Tree.cBalanced(4, 'x).toString()
    assert(expected == result)
  }

  test("Completely balanced tree with 7 nodes") {
    //only one valid solution
    val expected = "List(T('x T('x T('x . .) T('x . .)) T('x T('x . .) T('x . .))))"
    val result = Tree.cBalanced(7, 'x).toString()
    assert(expected == result)
  }

  test("Is symmetric") {
    assert(Node('a', Node('b'), Node('c')).isSymmetric)
    assert(Node('a', Node('b', Node('x')), Node('c', End, Node('x'))).isSymmetric)
    assert(!Node('a', Node('b', Node('x')), Node('b', Node('x'))).isSymmetric)
    assert(!Node('x', Node('x')).isSymmetric)
  }

  test("Add value") {
    assert(Node("value") == End.addValue("value"))
    assert(End.addValue(1).addValue(2) == Node(1, End, Node(2)))
    assert(End.addValue(10).addValue(2) == Node(10, Node(2)))
    assert(
      End.addValue(10).addValue(6).addValue(5).addValue(4) ==
        Node(10, Node(6, Node(5, Node(4))))
    )
    assert(
      End.addValue(10).addValue(6).addValue(12).addValue(4) ==
        Node(10, Node(6, Node(4)), Node(12)))
  }

  test("fromList") {
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
    assert(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric == false)
  }

  test("symmetricBalancedTrees") {
    val result = Tree.symmetricBalancedTrees(5, 'x).toString()
    val expected = "List(T('x T('x T('x . .) .) T('x . T('x . .))), T('x T('x . T('x . .)) T('x T('x . .) .)))"
    assert(expected == result)
  }

  test("heightBalancedTrees") {
    val result = Tree.hbalTrees(3, 'x)

    val expected0 = Node('x, Node('x,  Node('x),  Node('x )), Node('x, Node('x), Node('x)))
    assert(result(0) == expected0)

    val expected1 =  Node('x, Node('x, Node('x), Node('x)), Node('x, Node('x)))
    assert(result(1) == expected1)

    //todo: add rest
  }

  test("howManySolutionsExistsForNNodes") {
    assert(1 == Tree.getSolutionsNumber(15))
  }

  test("leafCount") {
    assert(1 == Tree.leafCount(Node('x', Node('x'), End)))
  }

  test("leafList") {
    val expected = List('b', 'd', 'e')
    val result = Tree.leafList(Node('a', Node('b'), Node('c', Node('d'), Node('e'))))
    assert(expected == result)
  }

  test("internalList") {
    val expected = List('a', 'c')
    val result = Tree.internalList(Node('a', Node('b'), Node('c', Node('d'), Node('e'))))
    assert(expected == result)
  }
}
