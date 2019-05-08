import scala.annotation.tailrec

/**
  * P01 (*) Find the last element of a list.
  * Example:
  * scala> last(List(1, 1, 2, 3, 5, 8))
  * res0: Int = 8
  */
object P01 extends App {
  @tailrec
  def last[A](data: List[A]): A = {
      if (data.tail.isEmpty) data.head
      else last(data.tail)
  }
}