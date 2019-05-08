/**
  * P13 (**) Run-length encoding of a list (direct solution).
  * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
  * Example:
  *
  * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  */
object P13 extends App {
  def encodeDirect(
    symbols: List[Symbol],
    current: List[Symbol] = List(),
    result: List[(Int, Symbol)] = List() //fixme: unnecessary parameter
  ): List[(Int, Symbol)] = {
    if (symbols.isEmpty) result :+ (current.length, current.head)
    else if (current.isEmpty) encodeDirect(symbols.tail, List() :+ symbols.head, result)
    else if (symbols.head.equals(current.head)) encodeDirect(symbols.tail, current :+ symbols.head, result)
    else encodeDirect(symbols.tail, List() :+ symbols.head, result :+ (current.length, current.head))
  }
}
