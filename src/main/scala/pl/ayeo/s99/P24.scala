package pl.ayeo.s99

/**
  * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  * Example:
  * scala> lotto(6, 49)
  * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  */
object P24 extends App {
  implicit class Tuple(t: (List[Int], Int)) {
    def +(p: (List[Int], Int)): (List[Int], Int) = (t._1 ::: p._1, if (p._2 == 0) t._2 else p._2)
  }

  def removeAt(position: Int, list: List[Int]): (List[Int], Int) = {
    def helper(position: Int, data: (List[Int], Int)): (List[Int], Int) = {
      val (list, removed) = data
      if (list.isEmpty) (Nil, 0)
      else if (position == 0) helper(-1, (list.tail, list.head))
      else (List(list.head), removed) + helper(position - 1, (list.tail, removed))
    }

    helper(position, (list, 0))
  }

  def range(from: Int, to: Int): List[Int] = {
    def helper(from: Int, to: Int): List[Int] = {
      if (to < 0) Nil
      else from +: helper(from + 1, to - 1)
    }

    helper(from, to - from)
  }

  def lotto(quantity: Int, maxNumber: Int): List[Int] = {
    val allNumbers = range(1, maxNumber)

    def helper(quantity: Int, numbers: List[Int]): List[Int] = {
      if (quantity == 0 || numbers.isEmpty) Nil
      else if (numbers.length == 1) List(numbers.head)
      else {
        val (list, number) = removeAt(scala.util.Random.nextInt(numbers.length - 1), numbers)
        number +: helper(quantity - 1, list)
      }
    }

    helper(quantity + 1, allNumbers)
  }
}
