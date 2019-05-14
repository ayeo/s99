package pl.ayeo.s99

import com.sun.javaws.exceptions.InvalidArgumentException

class ArithmeticInt(private val i: Int) {
  /**
    * P31 (**) Determine whether a given integer number is prime.
    * scala> 7.isPrime
    * res0: Boolean = true
    *
    * Note: this is implemented within ArithmeticInt class
    */
  def isPrime: Boolean = {
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
    *
    * Note: this is implemented within ArithmeticInt class
    */
  def isCoprimeTo(a: Int): Boolean = {
    try
      ArithmeticInt.gcd(i, a) == 1
    catch {
      case e: IllegalArgumentException => false
    }
  }

  /**
    * P34 (**) Calculate Euler's totient function phi(m).
    * Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
    * scala> 10.totient
    * res0: Int = 4
    */
  def totient(): Int = {
    def helper(a: Int): Int = {
      if (a.equals(1)) 1
      else if (a.isCoprimeTo(i)) 1 + helper(a - 1)
      else helper(a - 1)
    }

    if (i < 1) throw new IllegalArgumentException()
    else helper(i)
  }
}

object ArithmeticInt {
  /**
    * P33 (*) Determine whether two positive integer numbers are coprime.
    * Two numbers are coprime if their greatest common divisor equals 1.
    * scala> 35.isCoprimeTo(64)
    * res0: Boolean = true
    *
    * Note: this is implemented within ArithmeticInt class
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