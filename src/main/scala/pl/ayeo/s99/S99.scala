package pl.ayeo.s99

object S99 extends App {
  val set1 = new WorkingWithLists

  //P01: (*) Find the last element of a list.
  println(set1.last(List(1, 1, 2, 3, 5, 8)))

  //P02: (*) Find the last but one element of a list.
  println(set1.penultimate(List(1, 1, 2, 3, 5, 8)))

  //P03: (*) Find the Kth element of a list.
  println(set1.nth(2, List(1, 1, 2, 3, 5, 8)))

  //P04: (*) Find the number of elements of a list.
  println(set1.length(List(1, 1, 2, 3, 5, 8)))

  //P05: (*) Reverse a list.
  println(set1.reverse(List(1, 1, 2, 3, 5, 8)))

  //P06: (*) Find out whether a list is a palindrome.
  println(set1.isPalindrome(List(1, 2, 3, 2, 1)))

  //P07: (**) Flatten a nested list structure.
  println(set1.flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
}
