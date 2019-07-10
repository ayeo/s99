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
    val expected = "List(T('x T('x . .) T('x . T('x . .))), T('x T('x . .) T('x T('x . .) .)), T('x T('x . T('x . .)) T('x . .)), T('x T('x T('x . .) .) T('x . .)))"
    val result = Tree.cBalanced(4, 'x).toString()
    assert(expected == result)
  }

  test("Completely balanced tree with 7 nodes") {
    //only one valid solution
    val expected = "List(T('x T('x T('x . .) T('x . .)) T('x T('x . .) T('x . .))))"
    val result = Tree.cBalanced(7, 'x).toString()
    assert(expected == result)
  }
}
