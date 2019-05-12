package pl.ayeo.s99

import java.security.InvalidParameterException

import scala.annotation.tailrec
import scala.util.Random

class WorkingWithLists {
  implicit class Tuple[A](t: (List[A], A)) {
    def +(p: (List[A], A)): (List[A], A) = (t._1 ::: p._1, if (p._2 == null) t._2 else p._2)
  }

  /**
    * P01 (*) Find the last element of a list.
    * Example:
    * scala> last(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 8
    */
  @tailrec
  @throws(classOf[IllegalArgumentException])
  final def last[A](data: List[A]): A = {
    if (data.isEmpty) throw new IllegalArgumentException
    else if (data.tail.isEmpty) data.head
    else last(data.tail)
  }

  /**
    * P02 (*) Find the last but one element of a list.
    * Example:
    * scala> penultimate(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 5
    *
    * Note:
    * This may use nth(list.length - 2, list) but this is before P03
    */
  @tailrec
  final def penultimate[A](data: List[A], result: A = null): A = {
    if (data.isEmpty || data.tail.isEmpty) {
      if (result == null) throw new InvalidParameterException()
      else result
    } else penultimate(data.tail, data.head)
  }

  /**
    * P03 (*) Find the Kth element of a list.
    * By convention, the first element in the list is element 0.
    * Example:
    *
    * scala> nth(2, List(1, 1, 2, 3, 5, 8))
    * res0: Int = 2
    */
  @throws(classOf[InvalidParameterException])
  @tailrec
  final def nth[A](counter: Int, data: List[A]): A = {
    if (data.isEmpty) throw new InvalidParameterException()
    else if (counter.equals(0)) data.head
    else nth(counter - 1, data.tail)
  }

  /**
    * P04 (*) Find the number of elements of a list.
    * Example:
    * scala> length(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 6
    */
  final def length(data: List[Any]): Int = {
    if (data.isEmpty) 0
    else length(data.tail) + 1
  }

  /**
    * P05 (*) Reverse a list.
    * Example:
    * scala> reverse(List(1, 1, 2, 3, 5, 8))
    * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
    */
  @tailrec
  final def reverse[A](input: List[A], output: List[A] = List()): List[A] = {
    if (input.isEmpty) output
    else reverse(input.tail, input.head +: output)
  }

  /**
    * P06 (*) Find out whether a list is a palindrome.
    * Example:
    * scala> isPalindrome(List(1, 2, 3, 2, 1))
    * res0: Boolean = true
    */
  final def isPalindrome[A](data: List[A]): Boolean = {
    data == reverse(data)
  }

  /**
    * P07 (**) Flatten a nested list structure.
    * Example:
    * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
    */
  final def flatten[A](list: List[A]): List[A] = {
    if (list.isEmpty) Nil //fixme: use pattern matching (did not know it then)
    else if (list.head.isInstanceOf[List[A]]) flatten(list.head.asInstanceOf[List[A]]) ::: flatten(list.tail)
    else list.head +: flatten(list.tail)
  }

  /**
    * P08 (**) Eliminate consecutive duplicates of list elements.
    * If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    * Example:
    *
    * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
    */
  def compress[A](list: List[A], last: A = null): List[A] = {
    if (list.isEmpty) Nil
    else if (list.head.equals(last)) compress(list.tail, last)
    else list.head +: compress(list.tail, list.head)
  }

  /**
    * P09 (**) Pack consecutive duplicates of list elements into sublists.
    * If a list contains repeated elements they should be placed in separate sublists.
    * Example:
    *
    * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    */
  def pack[A](list: List[A], sublist: List[A] = List()): List[List[A]] = {
    if (list.isEmpty) List() :+ sublist
    else if (sublist.nonEmpty) {
      if (sublist.head.equals(list.head)) pack(list.tail, sublist :+ list.head)
      else sublist :: pack(list.tail, List() :+ list.head)
    } else pack(list.tail, List() :+ list.head)
  }

  /**
    * P10 (*) Run-length encoding of a list.
    * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
    * Example:
    *
    * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    *
    * fixme: use tuple with generic type instead of Symbol
    */
  def encode(list: List[Symbol]): List[(Int, Symbol)] = {
    def helper(innerList: List[List[Symbol]], result: List[(Int, Symbol)] = List()): List[(Int, Symbol)] = {
      if (innerList.isEmpty) result
      else helper(innerList.tail, result ::: List((innerList.head.length, innerList.head.head)))
    }

    helper(pack(list))
  }

  /**
    * P11 (*) Modified run-length encoding.
    * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
    * Example:
    *
    * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
    *
    * fixme: use tuple with generic type instead of Symbol
    * fixme: how to specify more concrete return type? List[tuple] OR Symbol
    */
  def encodeModified(list: List[Symbol]): List[Any] = {
    def helper(innerList: List[List[Symbol]], result: List[Any] = List()): List[Any] = {
      if (innerList.isEmpty) result
      else {
        if (innerList.head.length.equals(1)) helper(innerList.tail, result :+ innerList.head.head)
        else helper(innerList.tail, result ::: List((innerList.head.length, innerList.head.head)))
      }
    }

    helper(pack(list))
  }


  /**
    * (**) Decode a run-length encoded list.
    *
    * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    * Example:
    *
    * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    */
  def decode(list: List[(Int, Symbol)]): List[Symbol] = {
    def expand(symbol: Symbol, counter: Int, result: List[Symbol] = List()): List[Symbol] = {
      if (counter.equals(0)) result
      else expand(symbol, counter - 1, result :+ symbol)
    }

    if (list.isEmpty) Nil
    else {
      val (counter, symbol) = list.head
      if (list.tail.isEmpty) expand(symbol, counter)
      else expand(symbol, counter) ::: decode(list.tail)
    }
  }

  /**
    * P13 (**) Run-length encoding of a list (direct solution).
    * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    * Example:
    *
    * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    */
  def encodeDirect(
    symbols: List[Symbol],
    current: List[Symbol] = List(),
    result: List[(Int, Symbol)] = List() //fixme: unnecessary parameter
  ): List[(Int, Symbol)] = {
    if (symbols.isEmpty) result :+ (current.length, current.head)
    else if (current.isEmpty) encodeDirect(symbols.tail, List() :+ symbols.head, result)
    else if (symbols.head.equals(current.head)) encodeDirect(symbols.tail, current :+ symbols.head, result)
    else encodeDirect(symbols.tail, List() :+ symbols.head, result :+ (current.length, current.head))
  }

  /**
    * P14 (*) Duplicate the elements of a list.
    * Example:
    * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
    * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    */
  def duplicate(list: List[Symbol], flag: Boolean = false): List[Symbol] = {
    if (list.isEmpty) Nil
    else if (flag) list.head +: duplicate(list.tail)
    else list.head +: duplicate(list, !flag)
  }


  /**
    * P15 (**) Duplicate the elements of a list a given number of times.
    * Example:
    * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    */
  def duplicateN(i: Int, list: List[Symbol]): List[Symbol] = {
    def helper(x: Int, list: List[Symbol]): List[Symbol] = {
      if (list.isEmpty) Nil
      else if (x.equals(0)) helper(i, list.tail)
      else list.head +: helper(x - 1, list)
    }

    helper(i, list)
  }

  /**
    * P16 (**) Drop every Nth element from a list.
    * Example:
    * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    */
  def drop(i: Int, list: List[Symbol]): List[Symbol] = {
    def helper(x: Int, list: List[Symbol]): List[Symbol] = {
      if (list.isEmpty) Nil
      else if (x.equals(1)) helper(i, list.tail)
      else list.head +: helper(x - 1, list.tail)
    }

    helper(i, list)
  }

  /**
    * P17 (*) Split a list into two parts.
    * The length of the first part is given. Use a Tuple for your result.
    * Example:
    *
    * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    */
  def split(i: Int, list: List[Symbol], sublist: List[Symbol] = List()): List[List[Symbol]] = {
    if (list.isEmpty) List(sublist)
    else if (i > 0) split(i - 1, list.tail, sublist :+ list.head)
    else sublist :: split(list.length, list, List())
  }

  /**
    * P18 (**) Extract a slice from a list.
    * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
    * Example:
    *
    * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('d, 'e, 'f, 'g)
    */
  def slice(from: Int, to: Int, list: List[Symbol], result: List[Symbol] = List()): List[Symbol] = {
    if (to == 0 || list.isEmpty) result
    else if (from > 0) slice(from - 1, to - 1, list.tail)
    else slice(0, to - 1, list.tail, result :+ list.head)
  }

  /**
    * P19 (**) Rotate a list N places to the left.
    * Examples:
    * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    *
    * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    */
  def rotate(startPoint: Int, list: List[Symbol]): List[Symbol] = {
    val from = if (startPoint < 0) list.length + startPoint else startPoint
    val to = if (startPoint < 0) startPoint else list.length
    slice(from, to, list) ::: slice(0, from, list)
  }

  /**
    * P20 (*) Remove the Kth element from a list.
    * Return the list and the removed element in a Tuple. Elements are numbered from 0.
    * Example:
    *
    * scala> removeAt(1, List('a, 'b, 'c, 'd))
    * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
    */
  def removeAt(position: Int, list: List[Any]): (List[Any], Any) = {
    def helper(position: Int, data: (List[Any], Any)): (List[Any], Any) = {
      val (list, removed) = data
      if (list.isEmpty) (Nil, null)
      else if (position == 0) helper(-1, (list.tail, list.head))
      else (List(list.head), removed) + helper(position - 1, (list.tail, null))
    }

    helper(position, (list, null))
  }

  /**
    * P21 (*) Insert an element at a given position into a list.
    * Example:
    * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
    * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
    */
  def insertAt(newSymbol: Symbol, position: Int, list: List[Symbol]): List[Symbol] = {
    if (list.isEmpty) Nil
    else if (position == 0) newSymbol +: insertAt(newSymbol, -1, list)
    else list.head +: insertAt(newSymbol, position - 1, list.tail)
  }

  /**
    * P22 (*) Create a list containing all integers within a given range.
    * Example:
    * scala> range(4, 9)
    * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
    */
  def range(from: Int, to: Int): List[Int] = {
    def helper(from: Int, to: Int): List[Int] = {
      if (to < 0) Nil
      else from +: helper(from + 1, to - 1)
    }

    helper(from, to - from)
  }

  /**
    * P23 (**) Extract a given number of randomly selected elements from a list.
    * Example:
    * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
    * res0: List[Symbol] = List('e, 'd, 'a)
    * Hint: Use the solution to problem P20
    */
  def randomSelect(quantity: Int, symbols: List[Symbol]): List[Symbol] = {
    if (quantity <= 0) Nil
    else {
      val (list, removed) = removeAt(Random.nextInt(symbols.length - 1), symbols)
      removed.asInstanceOf[Symbol] +: randomSelect(quantity - 1, list.asInstanceOf[List[Symbol]])
    }
  }

  /**
    * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    * Example:
    * scala> lotto(6, 49)
    * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
    */
  def lotto(quantity: Int, maxNumber: Int): List[Int] = {
    def helper(quantity: Int, numbers: List[Int]): List[Int] = {
      if (quantity == 0 || numbers.isEmpty) Nil
      else if (numbers.length == 1) List(numbers.head)
      else {
        val (list, number) = removeAt(scala.util.Random.nextInt(numbers.length - 1), numbers)
        number.asInstanceOf[Int] +: helper(quantity - 1, list.asInstanceOf[List[Int]])
      }
    }

    helper(quantity, range(1, maxNumber))
  }
}

object WorkingWithLists {
   def apply(): WorkingWithLists = new WorkingWithLists
}
