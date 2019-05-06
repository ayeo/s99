//Pack consecutive duplicates of list elements into sublists.
object P09 extends App {
  def pack[A](list: List[A], sublist: List[A] = List()): List[Any] = {
    if (list.isEmpty) List() :+ sublist
    else if (!sublist.isEmpty) {
      if (sublist.head.equals(list.head)) pack(list.tail, sublist :+ list.head)
      else (sublist) :: pack(list.tail, List() :+ list.head)
    } else pack(list.tail, List() :+ list.head)
  }

  val result = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //expected: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  println(result)
}
