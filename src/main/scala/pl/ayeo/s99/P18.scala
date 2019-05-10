package pl.ayeo.s99

/**
  * P18 (**) Extract a slice from a list.
  * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
  * Example:
  *
  * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  * res0: List[Symbol] = List('d, 'e, 'f, 'g)
  */
object P18 extends App {
  def slice(from: Int, to: Int, list: List[Symbol], result: List[Symbol] = List()): List[Symbol] = {
    if (to == 0 || list.isEmpty) result
    else if (from > 0) slice(from - 1, to - 1, list.tail)
    else slice(0, to - 1, list.tail, result :+ list.head)
  }
}
