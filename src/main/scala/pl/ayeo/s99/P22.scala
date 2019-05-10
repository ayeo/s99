package pl.ayeo.s99

/**
  * P22 (*) Create a list containing all integers within a given range.
  * Example:
  * scala> range(4, 9)
  * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  */
object P22 extends App {
  def range(from: Int, to: Int): List[Int] = {
    def helper(from: Int, to: Int): List[Int] = {
      if (to < 0) Nil
      else from +: helper(from + 1, to - 1)
    }

    helper(from, to - from)
  }
}
