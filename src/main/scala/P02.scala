import scala.annotation.tailrec

/**
  * P02 (*) Find the last but one element of a list.
  * Example:
  * scala> penultimate(List(1, 1, 2, 3, 5, 8))
  * res0: Int = 5
  */
object P02 extends App {
  @tailrec
  def penultimate[A](data: List[A], result: Option[A] = None): Option[A] = {
    if (data.isEmpty || data.tail.isEmpty) result
    else penultimate(data.tail, Some(data.head))
  }
}
