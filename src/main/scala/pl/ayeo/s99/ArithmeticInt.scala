package pl.ayeo.s99

class ArithmeticInt(private val i: Int) {
  /**
    * P31 (**) Determine whether a given integer number is prime.
    * scala> 7.isPrime
    * res0: Boolean = true
    */
  final def isPrime: Boolean = {
    val sqrt = scala.math.sqrt(i)

    def helper(divider: Int): Boolean = {
      if (divider > sqrt) true
      else if (i % divider == 0 || i % (divider + 2) == 0) false
      else helper(divider + 6)
    }

    if (i <= 3) i > 1
    else if (i % 2 == 0 || i % 3 == 0) false
    else helper(5)
  }

  /**
    * P32 (**) Determine the greatest common divisor of two positive integer numbers.
    * Use Euclid's algorithm.
    * scala> gcd(36, 63)
    * res0: Int = 9
    */
  final def isCoprimeTo(a: Int): Boolean = {
    try
      ArithmeticInt.gcd(i, a) == 1
    catch {
      case e: IllegalArgumentException => false
    }
  }

  /**
    * P34 (**) Calculate Euler's totient function phi(m).
    * Euler's so-called totient function phi(m) is defined as the
    * number of positive integers r (1 <= r <= m) that are coprime to m.
    * scala> 10.totient
    * res0: Int = 4
    */
  final def totient(): Int = {
    def helper(a: Int): Int = {
      if (a.equals(1)) 1
      else if (a.isCoprimeTo(i)) 1 + helper(a - 1)
      else helper(a - 1)
    }

    if (i < 1) throw new IllegalArgumentException()
    else helper(i)
  }

  /**
    * (**) Determine the prime factors of a given positive integer.
    * Construct a flat list containing the prime factors in ascending order.
    * scala> 315.primeFactors
    * res0: List[Int] = List(3, 3, 5, 7)
    */
  final def primeFactors(): List[Int] = {
    def nextPrime(a: Int): Int = {
      if (a.isPrime) a
      else nextPrime(a + 1)
    }

    def helper(rest: Int, divider: Int): List[Int] = {
      if (rest.equals(1)) Nil
      else if (rest % divider == 0) divider +: helper(rest/divider, divider)
      else helper(rest, nextPrime(divider + 1))
    }

    helper(i, 2)
  }

  /**
    * P36 (**) Determine the prime factors of a given positive integer (2).
    * Construct a list containing the prime factors and their multiplicity.
    * scala> 315.primeFactorMultiplicity
    * res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
    * Alternately, use a Map for the result.
    *
    * scala> 315.primeFactorMultiplicity
    * res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
    */
  final def primeFactorMultiplicity(): List[(Int, Int)] = {
    def helper(symbols: List[Int], current: List[Int] = List()): List[(Int, Int)] = {
        if (symbols.isEmpty) {
        if (current.isEmpty) List()
        else List((current.head, current.length))
      }
      else if (current.isEmpty) helper(symbols.tail, List(symbols.head))
      else if (symbols.head.equals(current.head)) helper(symbols.tail, current :+ symbols.head)
      else List((current.head, current.length)) ::: helper(symbols.tail, List(symbols.head))
    }

    helper(i.primeFactors())
  }

  /**
    * P37 (**) Calculate Euler's totient function phi(m) (improved).
    * See problem P34 for the definition of Euler's totient function.
    * If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m>) c
    * an be efficiently calculated as follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime
    * factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
    * phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...
    *
    * Note that ab stands for the bth power of a.
    */
  final def phi(): Int = {
    def helper(factors: List[(Int, Int)]): Int = {
      if (factors.isEmpty) 1
      else {
        val (p, m): (Int, Int) = factors.head
        ((p - 1) * Math.pow(p, m - 1)).asInstanceOf[Int] * helper(factors.tail)
      }
    }

    helper(i.primeFactorMultiplicity())
  }
}

object ArithmeticInt {
  /**
    * P33 (*) Determine whether two positive integer numbers are coprime.
    * Two numbers are coprime if their greatest common divisor equals 1.
    * scala> 35.isCoprimeTo(64)
    * res0: Boolean = true
    */
  def gcd(a: Int, b: Int): Int = {
    def helper(a: Int, b: Int): Int = {
      if (a.equals(b)) a
      else if (a > b) helper(a - b, b)
      else helper(b - a, a)
    }

    if (a.equals(0) || b.equals(0)) throw new IllegalArgumentException()
    else helper(Math.abs(a), Math.abs(b))
  }
}