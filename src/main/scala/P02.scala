import scala.annotation.tailrec

//Find the last but one element of a list.
object P02 extends App {
  @tailrec
  def penultimate[A](data: List[A], result: Option[A] = None): Option[A] = {
    if (data.isEmpty || data.tail.isEmpty) result
    else penultimate(data.tail, Some(data.head))
  }

  val result =  penultimate(List(1, 1, 2, 3, 4, 5, 8))
  println(result)
}
