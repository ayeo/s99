package pl.ayeo.s99

/**
  * P08 (**) Eliminate consecutive duplicates of list elements.
  * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  * Example:
  *
  * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  */
object P08 extends App {
  def compress[A](list: List[A], last: A = null): List[A] = {
    if (list.isEmpty) Nil
    else if (list.head.equals(last)) compress(list.tail, last)
    else list.head +: compress(list.tail, list.head)
  }
}
