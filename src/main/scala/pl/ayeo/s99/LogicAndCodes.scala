package pl.ayeo.s99

class LogicAndCodes {
  def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def not(a: Boolean): Boolean = a match {
    case true => false
    case false => true
  }

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))
  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))
  def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))
  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)
  def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))

  /**
    * P46 (**) Truth tables for logical expressions.
    * Define functions and, or, nand, nor, xor, impl, and equ (for logical
    * equivalence) which return true or false according to the result of their
    * respective operations; e.g. and(A, B) is true if and only if both A and B
    * are true.
    *
    * scala> and(true, true)
    * res0: Boolean = true
    *
    * scala> xor(true. true)
    * res1: Boolean = false
    *
    * A logical expression in two variables can then be written as a function of
    * two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
    *
    * Now, write a function called table2 which prints the truth table of a
    * given logical expression in two variables.
    *
    * scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    * A     B     result
    * true  true  true
    * true  false true
    * false true  false
    * false false false
    *
    * The trick here is not using builtins.  We'll define `not`, `and`, and `or`
    * directly (using pattern matching), and the other functions in terms of those
    * three.
    */
  def table2(f: (Boolean, Boolean) => Boolean): Unit = {
    println("A\t\tB\t\tresult")
    for {a <- List(true, false); b <- List(true, false)} {
      printf("%s\t%s\t%s\n", a, b, f(a, b))
    }
  }

  /**
    * P49 (**) Gray code.
    * An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
    * n = 1: C(1) = ("0", "1").
    * n = 2: C(2) = ("00", "01", "11", "10").
    * n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
    * Find out the construction rules and write a function to generate Gray codes.
    *
    * scala> gray(3)
    * res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
    * See if you can use memoization to make the function more efficient.
    */
  def grey(b: Int): List[String] = b match {
    case 0 => List("")
    case _ => grey(b - 1).map("0" + _) ::: grey(b - 1).reverse.map("1" + _)
  }
}

object LogicAndCodes {
  def apply(): LogicAndCodes = new LogicAndCodes()
}
