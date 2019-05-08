import scala.annotation.tailrec

/**
  * P06 (*) Find out whether a list is a palindrome.
  * Example:
  * scala> isPalindrome(List(1, 2, 3, 2, 1))
  * res0: Boolean = true
  */
object P06 extends App {
  def isPalindrome[A](data: List[A]): Boolean = {
    @tailrec
    def reverse(input: List[A], output: List[A] = List()): List[A] = {
      if (input.isEmpty) output
      else reverse(input.tail, input.head +: output)
    }

    data == reverse(data)
  }
}
