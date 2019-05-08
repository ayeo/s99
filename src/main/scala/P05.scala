import scala.annotation.tailrec

/**
  * P05 (*) Reverse a list.
  * Example:
  * scala> reverse(List(1, 1, 2, 3, 5, 8))
  * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  */
object P05 extends App {
  @tailrec
  def reverse[A](input: List[A], output: List[A] = List()): List[A] = {
      if (input.isEmpty) output
      else reverse(input.tail, input.head +: output)
  }
}
