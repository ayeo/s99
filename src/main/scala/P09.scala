/**
  * P09 (**) Pack consecutive duplicates of list elements into sublists.
  * If a list contains repeated elements they should be placed in separate sublists.
  * Example:
  *
  * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */
object P09 extends App {
  def pack[A](list: List[A], sublist: List[A] = List()): List[List[A]] = {
    if (list.isEmpty) List() :+ sublist
    else if (sublist.nonEmpty) {
      if (sublist.head.equals(list.head)) pack(list.tail, sublist :+ list.head)
      else sublist :: pack(list.tail, List() :+ list.head)
    } else pack(list.tail, List() :+ list.head)
  }
}
