package pl.ayeo.s99

/**
  * P21 (*) Insert an element at a given position into a list.
  * Example:
  * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  */
object P21 extends App {
  def insertAt(newSymbol: Symbol, position: Int, list: List[Symbol]): List[Symbol] = {
    if (list.isEmpty) Nil
    else if (position == 0) newSymbol +: insertAt(newSymbol, -1, list)
    else list.head +: insertAt(newSymbol, position - 1, list.tail)
  }
}
