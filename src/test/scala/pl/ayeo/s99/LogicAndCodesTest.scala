package pl.ayeo.s99

import org.scalatest.FunSuite

class LogicAndCodesTest extends FunSuite {
  private val processor: LogicAndCodes = LogicAndCodes()

  test("Test logic operator: and") {
    assert(true == processor.and(true, true))
    assert(false == processor.and(true, false))
    assert(false == processor.and(false, false))
    assert(false ==  processor.and(false, true))
  }

  test("Test logic operator: or") {
    assert(true == processor.or(true, true))
    assert(true == processor.or(true, false))
    assert(false == processor.or(false, false))
    assert(true ==  processor.or(false, true))
  }

  test("Test logic operator: not") {
    assert(true == processor.not(false))
    assert(false == processor.not(true))
  }

  test("Test logic operator: nand") {
    assert(false == processor.nand(true, true))
    assert(true == processor.nand(true, false))
    assert(true == processor.nand(false, false))
    assert(true ==  processor.nand(false, true))
  }

  test("Test logic operator: nor") {
    assert(false == processor.nor(true, true))
    assert(false == processor.nor(true, false))
    assert(true == processor.nor(false, false))
    assert(false ==  processor.nor(false, true))
  }

  test("Test logic operator: equ") {
    assert(true == processor.equ(true, true))
    assert(false == processor.equ(true, false))
    assert(true == processor.equ(false, false))
    assert(false ==  processor.equ(false, true))
  }

  test("Test logic operator: impl") {
    assert(true == processor.impl(true, true))
    assert(false == processor.impl(true, false))
    assert(true == processor.impl(false, false))
    assert(true ==  processor.impl(false, true))
  }

  test("Test logic operator: xor") {
    assert(false == processor.xor(true, true))
    assert(true == processor.xor(true, false))
    assert(false == processor.xor(false, false))
    assert(true ==  processor.xor(false, true))
  }

  test("Test custom add operator") {
    assert(true == (true and true))
    assert(false == (true and false))
    assert(false == (false and false))
    assert(false == (false and true))
  }

  test("Test custom or operator") {
    assert(true == (true or true))
    assert(true == (true or false))
    assert(false == (false or false))
    assert(true == (false or true))
  }

  test("Test custom nand operator") {
    assert(false == (true nand true))
    assert(true == (true nand false))
    assert(true == (false nand false))
    assert(true == (false nand true))
  }

  test("Test custom nor operator") {
    assert(false == (true nor true))
    assert(false == (true nor false))
    assert(true == (false nor false))
    assert(false == (false nor true))
  }

  test("Test custom equ operator") {
    assert(true == (true equ true))
    assert(false == (true equ false))
    assert(true == (false equ false))
    assert(false == (false equ true))
  }

  test("Test custom impl operator") {
    assert(true == (true impl true))
    assert(false == (true impl false))
    assert(true == (false impl false))
    assert(true == (false impl true))
  }

  test("Test custom xor operator") {
    assert(false == (true xor true))
    assert(true == (true xor false))
    assert(false == (false xor false))
    assert(true == (false xor true))
  }
}
