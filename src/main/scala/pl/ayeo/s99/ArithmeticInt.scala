package pl.ayeo.s99

class ArithmeticInt(private val i: Int) {
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

  def isCoprimeTo(a: Int): Boolean = {
    try
      ArithmeticInt.gcd(i, a) == 1
    catch {
      case e: IllegalArgumentException => false
    }
  }
}


object ArithmeticInt {
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