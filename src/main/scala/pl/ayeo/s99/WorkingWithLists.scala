package pl.ayeo.s99

import java.security.InvalidParameterException
import scala.annotation.tailrec

class WorkingWithLists {
  /**
    * P01 (*) Find the last element of a list.
    * Example:
    * scala> last(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 8
    */
  @tailrec
  @throws(classOf[IllegalArgumentException])
  final def last[A](data: List[A]): A = {
    if (data.isEmpty) throw new IllegalArgumentException
    else if (data.tail.isEmpty) data.head
    else last(data.tail)
  }

  /**
    * P02 (*) Find the last but one element of a list.
    * Example:
    * scala> penultimate(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 5
    *
    * Note:
    * This may use nth(list.length - 2, list) but this is before P03
    */
  @tailrec
  final def penultimate[A](data: List[A], result: A = null): A = {
    if (data.isEmpty || data.tail.isEmpty) {
      if (result == null) throw new InvalidParameterException()
      else result
    }
    else penultimate(data.tail, data.head)
  }

  /**
    * P03 (*) Find the Kth element of a list.
    * By convention, the first element in the list is element 0.
    * Example:
    *
    * scala> nth(2, List(1, 1, 2, 3, 5, 8))
    * res0: Int = 2
    */
  @throws(classOf[InvalidParameterException])
  @tailrec
  final def nth[A](counter: Int, data: List[A]): A = {
    if (data.isEmpty) throw new InvalidParameterException()
    else if (counter.equals(0)) data.head
    else nth(counter - 1, data.tail)
  }

  /**
    * P04 (*) Find the number of elements of a list.
    * Example:
    * scala> length(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 6
    */
  final def length(data: List[Any]): Int = {
    if (data.isEmpty) 0
    else length(data.tail) + 1
  }

  /**
    * P05 (*) Reverse a list.
    * Example:
    * scala> reverse(List(1, 1, 2, 3, 5, 8))
    * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
    */
  @tailrec
  final def reverse[A](input: List[A], output: List[A] = List()): List[A] = {
    if (input.isEmpty) output
    else reverse(input.tail, input.head +: output)
  }

  /**
    * P06 (*) Find out whether a list is a palindrome.
    * Example:
    * scala> isPalindrome(List(1, 2, 3, 2, 1))
    * res0: Boolean = true
    */
  final def isPalindrome[A](data: List[A]): Boolean = {
    data == reverse(data)
  }

  /**
    * P07 (**) Flatten a nested list structure.
    * Example:
    * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    */
  final def flatten[A](list: List[A]): List[A] = {
    if (list.isEmpty) Nil //fixme: use pattern matching (did not know it then)
    else if (list.head.isInstanceOf[List[A]]) flatten(list.head.asInstanceOf[List[A]]) ::: flatten(list.tail)
    else list.head +: flatten(list.tail)
  }
}

object WorkingWithLists {
   def apply(): WorkingWithLists = new WorkingWithLists
}
