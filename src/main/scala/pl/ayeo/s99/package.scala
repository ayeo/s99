package pl.ayeo

package object s99 {
  implicit def intToArithmeticInt(s: Int): ArithmeticInt = new ArithmeticInt(s)
}
