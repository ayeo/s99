import scala.annotation.tailrec

/**
  * P03 (*) Find the Kth element of a list.
  * By convention, the first element in the list is element 0.
  * Example:
  *
  * scala> nth(2, List(1, 1, 2, 3, 5, 8))
  * res0: Int = 2
  */
object P03 extends App {
  @tailrec
  def nth[A](counter: Int, data: List[A]): Option[A] = {
    if (data.isEmpty) None
    else if (counter.equals(0)) Some(data.head)
    else nth(counter - 1, data.tail)
  }
}
