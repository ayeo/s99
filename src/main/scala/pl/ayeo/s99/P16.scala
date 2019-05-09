package pl.ayeo.s99

/**
  * P16 (**) Drop every Nth element from a list.
  * Example:
  * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  */
object P16 extends App {
  def drop(i: Int, list: List[Symbol]): List[Symbol] = {
    def helper(x: Int, list: List[Symbol]): List[Symbol] = {
      if (list.isEmpty) Nil
      else if (x.equals(1)) helper(i, list.tail)
      else list.head +: helper(x - 1, list.tail)
    }

    helper(i, list)
  }
}
