package pl.ayeo.s99

class ArithmeticInt(private val i: Int) {
  def isPrime: Boolean = {
    def helper(int: Int): Boolean = {
      if (i % 2 == 0 || i % 3 == 0) false
      else if (int >= i) true
      else if (i % int == 0) false
      else helper(int + 6)
    }

    if (i <= 3) i > 1
    else helper(5)
  }
}
