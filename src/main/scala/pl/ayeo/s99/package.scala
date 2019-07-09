package pl.ayeo

package object s99 {
  implicit def intToArithmeticInt(s: Int): ArithmeticInt = new ArithmeticInt(s)
  implicit def booleanToSmartBoolean(b: Boolean): SmartBoolean = new SmartBoolean(b)

  /**
    * P47 (*) Truth tables for logical expressions (2).
    * Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit conversion from Boolean.) not will have to be left as a object method.
    * scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
    * A     B     result
    * true  true  true
    * true  false true
    * false true  false
    * false false false
    */
  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A\t\tB\t\tresult")
    for {a <- List(true, false); b <- List(true, false)} {
      printf("%s\t%s\t%s\n", a, b, f(a, b))
    }
  }

  def not(a: Boolean): Boolean = a match {
    case true => false
    case false => true
  }
}
