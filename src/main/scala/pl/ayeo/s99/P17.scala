package pl.ayeo.s99

/**
  * P17 (*) Split a list into two parts.
  * The length of the first part is given. Use a Tuple for your result.
  * Example:
  *
  * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  */
object P17 extends App {
  def split(i: Int, list: List[Symbol], sublist: List[Symbol] = List()): List[List[Symbol]] = {
    if (list.isEmpty) List(sublist)
    else if (i > 0) split(i - 1, list.tail, sublist :+ list.head)
    else sublist :: split(list.length, list, List())
  }
}
