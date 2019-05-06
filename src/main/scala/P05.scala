import scala.annotation.tailrec

//Reverse a list.
object P05 extends App {
  @tailrec
  def reverse[A](input: List[A], output: List[A] = List()): List[A] = {
      if (input.isEmpty) output
      else reverse(input.tail, input.head +: output)
  }

  val result = reverse(List(1, 1, 2, 3, 5, 8))
  println(result)
}
