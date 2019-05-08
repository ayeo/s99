package pl.ayeo.s99

/**
  * (**) Decode a run-length encoded list.
  *
  * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  * Example:
  *
  * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  */
object P12 extends App {
  def expand(symbol: Symbol, counter: Int, result: List[Symbol] = List()): List[Symbol] = {
    if (counter.equals(0)) result
    else expand(symbol, counter - 1, result :+ symbol)
  }

  def decode(list: List[(Int, Symbol)]): List[Symbol] = {
    if (list.isEmpty) Nil
    else {
      val (counter, symbol) = list.head
      if (list.tail.isEmpty) {
        expand(symbol, counter)
      } else {
        expand(symbol, counter) ::: decode(list.tail)
      }
    }
  }
}
