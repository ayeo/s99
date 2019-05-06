//Find the number of elements of a list.
object P04 extends App {
  def length(data: List[Any]): Int = {
    if (data.isEmpty) 0
    else length(data.tail) + 1
  }

  val result = length(List(1, 1, 2, 3, 5, 8))
  println(result)
}
