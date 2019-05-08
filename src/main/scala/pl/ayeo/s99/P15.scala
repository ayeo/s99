package pl.ayeo.s99

/**
  * P15 (**) Duplicate the elements of a list a given number of times.
  * Example:
  * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  */
object P15 extends App {
  def duplicateN(i: Int, list: List[Symbol]): List[Symbol] = {
    def helper(x: Int, list: List[Symbol]): List[Symbol] = {
      if (list.isEmpty) Nil
      else if (x.equals(0)) helper(i, list.tail)
      else list.head +: helper(x - 1, list)
    }

    helper(i, list)
  }
}
