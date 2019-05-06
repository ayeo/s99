import scala.annotation.tailrec

//Find the Kth element of a list.
object P03 extends App {
  @tailrec
  def nth[A](counter: Int, data: List[A]): Option[A] = {
    if (data.isEmpty) None
    else if (counter.equals(0)) Some(data.head)
    else nth(counter - 1, data.tail)
  }

  val result = nth(4, List(1, 1, 2, 3, 5, 8))
  println(result)
}
