/**
  * P14 (*) Duplicate the elements of a list.
  * Example:
  * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  */
object P14 extends App {
  def duplicate(list: List[Symbol], flag: Boolean = false): List[Symbol] = {
      if (list.isEmpty) Nil
      else if (flag) list.head +: duplicate(list.tail)
      else list.head +: duplicate(list, !flag)
  }

  val result = duplicate(List('a, 'b, 'c, 'c, 'd))
  println(result)
}
