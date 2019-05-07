//Run-length encoding of a list.
object P10 extends App {
  //fixme: use tuple with generic type instead of Symbol
  def encode(list: List[Symbol]): List[(Int, Symbol)] = {
    def pack[A](list: List[A], sublist: List[A] = List()): List[List[A]] = {
      if (list.isEmpty) List() :+ sublist
      else if (sublist.nonEmpty) {
        if (sublist.head.equals(list.head)) pack(list.tail, sublist :+ list.head)
        else sublist :: pack(list.tail, List() :+ list.head)
      } else pack(list.tail, List() :+ list.head)
    }

    def helper(innerList: List[List[Symbol]], result: List[(Int, Symbol)] = List()): List[(Int, Symbol)] = {
      if (innerList.isEmpty) result
      else helper(innerList.tail, result ::: List((innerList.head.length, innerList.head.head)))
    }

    helper(pack(list))
  }

  val result = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //expected = List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  println(result)
}
