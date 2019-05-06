import scala.annotation.tailrec

//Find the last element of a list.
object P01 extends App {
  @tailrec
  def last[A](data: List[A]): A = {
      if (data.tail.isEmpty) data.head
      else last(data.tail)
  }

  val result =  last(List(1, 1, 2, 3, 5, 8))
  println(result)
}