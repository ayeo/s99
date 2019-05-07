//Modified run-length encoding.
object P11 extends App {
  //fixme: use tuple with generic type instead of Symbol
  //fixme: how to specify more concrete return type? List[tuple] OR Symbol
  def encodeModified(list: List[Symbol]): List[Any] = {
    def pack[A](list: List[A], sublist: List[A] = List()): List[List[A]] = {
      if (list.isEmpty) List() :+ sublist
      else if (sublist.nonEmpty) {
        if (sublist.head.equals(list.head)) pack(list.tail, sublist :+ list.head)
        else sublist :: pack(list.tail, List() :+ list.head)
      } else pack(list.tail, List() :+ list.head)
    }

    def helper(innerList: List[List[Symbol]], result: List[Any] = List()): List[Any] = {
      if (innerList.isEmpty) result
      else {
        if (innerList.head.length.equals(1)) helper(innerList.tail, result :+ innerList.head.head)
        else helper(innerList.tail, result ::: List((innerList.head.length, innerList.head.head)))
      }
    }

    helper(pack(list))
  }


  val result = encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  println(result)
}
