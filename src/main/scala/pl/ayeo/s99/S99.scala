package pl.ayeo.s99

object S99 extends App {
  val set1 = new Set1

  //p01
  println(set1.last(List(1, 1, 2, 3, 5, 8)))

  //p02
  println(set1.penultimate(List(1, 1, 2, 3, 5, 8)))

  //p03
  println(set1.nth(2, List(1, 1, 2, 3, 5, 8)))

  //p04
  println(set1.length(List(1, 1, 2, 3, 5, 8)))

  //p05
  println(set1.reverse(List(1, 1, 2, 3, 5, 8)))

  //p06
  println(set1.isPalindrome(List(1, 2, 3, 2, 1)))
}
