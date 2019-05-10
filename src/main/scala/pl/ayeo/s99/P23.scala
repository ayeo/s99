package pl.ayeo.s99

import scala.util.Random

/**
  * P23 (**) Extract a given number of randomly selected elements from a list.
  * Example:
  * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  * res0: List[Symbol] = List('e, 'd, 'a)
  * Hint: Use the solution to problem P20
  */
object P23 extends App {
  implicit class Tuple(t: (List[Symbol], Symbol)) {
    def +(p: (List[Symbol], Symbol)): (List[Symbol], Symbol) = (t._1 ::: p._1, if (p._2 == null) t._2 else p._2)
  }

  def removeAt(position: Int, list: List[Symbol]): (List[Symbol], Symbol) = {
    def helper(position: Int, data: (List[Symbol], Symbol)): (List[Symbol], Symbol) = {
      val (list, removed) = data
      if (list.isEmpty) (Nil, null)
      else if (position == 0) helper(-1, (list.tail, list.head))
      else (List(list.head), removed) + helper(position - 1, (list.tail, null))
    }

    helper(position, (list, null))
  }

  def randomSelect(quantity: Int, symbols: List[Symbol]): List[Symbol] = {
    if (quantity <= 0) Nil
    else {
      val (list, removed) = removeAt(Random.nextInt(symbols.length - 1), symbols)
      removed +: randomSelect(quantity - 1, list)
    }
  }

  println(randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
}
