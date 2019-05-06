//Flatten a nested list structure.
object P07 extends App {
  def flatten[A](list: List[A]): List[A] = {
    if (list.isEmpty) Nil
    else if (list.head.isInstanceOf[List[A]]) flatten(list.head.asInstanceOf[List[A]]) ::: flatten(list.tail)
    else list.head +: flatten(list.tail)
  }

  val result = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  println(result)
}
