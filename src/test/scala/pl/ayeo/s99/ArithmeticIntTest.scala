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
      assert(ArithmeticInt.gcd(19, 0) == 1)
    }
  }

  test("gcd with zero as second parameter") {
    intercept[IllegalArgumentException] {
      assert(ArithmeticInt.gcd(0, 10) == 1)
    }
  }

  test("gcd with zero as both parameters") {
    intercept[IllegalArgumentException] {
      assert(ArithmeticInt.gcd(0, 0) == 1)
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

}
