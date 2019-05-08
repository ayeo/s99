/**
  * P07 (**) Flatten a nested list structure.
  * Example:
  * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  */
object P07 extends App {
  def flatten[A](list: List[A]): List[A] = {
    if (list.isEmpty) Nil //fixme: use pattern matching (did not know it then)
    else if (list.head.isInstanceOf[List[A]]) flatten(list.head.asInstanceOf[List[A]]) ::: flatten(list.tail)
    else list.head +: flatten(list.tail)
  }
}
