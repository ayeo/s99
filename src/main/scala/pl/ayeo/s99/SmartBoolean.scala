package pl.ayeo.s99

class SmartBoolean(private val a: Boolean) {
  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def nand(b: Boolean): Boolean = not(a and b)
  def nor(b: Boolean): Boolean = not(a or b)
  def equ(b: Boolean): Boolean = (a and b) or (not(a) and not(b))
  def impl(b: Boolean): Boolean = not(a) or b
  def xor(b: Boolean): Boolean = not(a equ b)
}
