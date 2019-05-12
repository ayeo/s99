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
}
