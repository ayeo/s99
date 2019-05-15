package pl.ayeo.s99

import org.scalatest.FunSuite

class ArithmeticIntTest extends FunSuite {
  //P31
  test("isPrime") {
    assert(0.isPrime == false)
    assert(1.isPrime == false)
    assert(2.isPrime == true)
    assert(3.isPrime == true)
    assert(4.isPrime == false)
    assert(5.isPrime == true)
    assert(6.isPrime == false)
    assert(7.isPrime == true)
    assert(8.isPrime == false)
    assert(9.isPrime == false)
    assert(10.isPrime == false)
    assert(11.isPrime == true)
    assert(12.isPrime == false)
    assert(13.isPrime == true)
    assert(14.isPrime == false)

    assert(13.isPrime == true)
    assert(19.isPrime == true)
    assert(25.isPrime == false)

    assert(5273.isPrime == true)
    assert(7529.isPrime == true)
    assert(1019.isPrime == true)
    assert(6359.isPrime == true)
    assert(3457.isPrime == true)
    assert(3631.isPrime == true)
    assert(2693.isPrime == true)

    assert(1000.isPrime == false)
    assert(1001.isPrime == false)
    assert(1002.isPrime == false)
    assert(1003.isPrime == false)
    assert(1004.isPrime == false)
    assert(1005.isPrime == false)
    assert(1006.isPrime == false)
    assert(1007.isPrime == false)
    assert(1008.isPrime == false)
  }

  //P32
  test("gcd") {
    assert(ArithmeticInt.gcd(1, 1) == 1)

    assert(ArithmeticInt.gcd(36, 63) == 9)
    assert(ArithmeticInt.gcd(63, 36) == 9)

    assert(ArithmeticInt.gcd(1989, 867) == 51)
    assert(ArithmeticInt.gcd(867, 1989) == 51)

    assert(ArithmeticInt.gcd(120, 670) == 10)
    assert(ArithmeticInt.gcd(670, 120) == 10)

    assert(ArithmeticInt.gcd(-1083, 399) == 57)
  }

  test("gcd with zero as first parameter") {
    intercept[IllegalArgumentException] {
      ArithmeticInt.gcd(19, 0)
    }
  }

  test("gcd with zero as second parameter") {
    intercept[IllegalArgumentException] {
      ArithmeticInt.gcd(0, 10)
    }
  }

  test("gcd with zero as both parameters") {
    intercept[IllegalArgumentException] {
      ArithmeticInt.gcd(0, 0)
    }
  }

  //P33
  test("isCoprimeTo") {
    assert(35.isCoprimeTo(64) == true)
    assert(17.isCoprimeTo(52) == true)
    assert(17.isCoprimeTo(34) == false)
  }

  test("isCoprimeTo zero") {
    assert(1.isCoprimeTo(0) == false)
  }

  //P34
  test("totient with natural nubmers") {
    assert(1.totient == 1)
    assert(2.totient == 1)
    assert(3.totient == 2)
    assert(4.totient == 2)
    assert(5.totient == 4)
    assert(6.totient == 2)
    assert(7.totient == 6)
    assert(8.totient == 4)
    assert(9.totient == 6)
    assert(10.totient == 4)
    assert(11.totient == 10)
    assert(12.totient == 4)
  }

  test("totient with zero") {
    intercept[IllegalArgumentException] {
      0.totient
    }
  }

  test("totient with negative number") {
    intercept[IllegalArgumentException] {
      -7.totient
    }
  }

  //P35
  test("primeFactors") {
    assert(315.primeFactors == List(3, 3, 5, 7))
    assert(27.primeFactors == List(3, 3, 3))
    assert(3432.primeFactors == List(2, 2, 2, 3, 11, 13))
  }

  //P36
  test("primeFactorsMultiplicity") {
    assert(315.primeFactorMultiplicity == List((3,2), (5,1), (7,1)))
    assert(27.primeFactorMultiplicity == List((3, 3)))
    assert(3432.primeFactorMultiplicity == List((2, 3), (3, 1), (11, 1), (13, 1)))
  }
}
