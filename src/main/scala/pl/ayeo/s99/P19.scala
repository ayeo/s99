package pl.ayeo.s99

/**
  * P19 (**) Rotate a list N places to the left.
  * Examples:
  * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  *
  * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  */
object P19 extends App {
  def slice(from: Int, to: Int, list: List[Symbol], result: List[Symbol] = List()): List[Symbol] = {
    if (to == 0 || list.isEmpty) result
    else if (from > 0) slice(from - 1, to - 1, list.tail)
    else slice(0, to - 1, list.tail, result :+ list.head)
  }

  def rotate(startPoint: Int, list: List[Symbol]): List[Symbol] = {
    val from = if (startPoint < 0) list.length + startPoint else startPoint
    val to = if (startPoint < 0) startPoint else list.length
    slice(from, to, list) ::: slice(0, from, list)
  }
}
