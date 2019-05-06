import scala.annotation.tailrec

//Find out whether a list is a palindrome.
object P06 extends App {
  def isPalindrome[A](data: List[A]): Boolean = {
    @tailrec
    def reverse[A](input: List[A], output: List[A] = List()): List[A] = {
      if (input.isEmpty) output
      else reverse(input.tail, input.head +: output)
    }

    return data == reverse(data)
  }

  val result = isPalindrome(List(1, 2, 3, 2, 1))
  println(result)
}
