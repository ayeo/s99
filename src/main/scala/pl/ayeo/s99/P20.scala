package pl.ayeo.s99

/**
  * P20 (*) Remove the Kth element from a list.
  * Return the list and the removed element in a Tuple. Elements are numbered from 0.
  * Example:
  *
  * scala> removeAt(1, List('a, 'b, 'c, 'd))
  * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  */
object P20 extends App {
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
}
