//Eliminate consecutive duplicates of list elements.
object P08 extends App {
  def compress[A](list: List[A], last: A = null): List[A] = {
    if (list.isEmpty) Nil
    else if (list.head.equals(last)) compress(list.tail, last)
    else list.head +: compress(list.tail, list.head)
  }

  val result = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //expected: List('a, 'b, 'c, 'a, 'd, 'e)
  println(result)
}
