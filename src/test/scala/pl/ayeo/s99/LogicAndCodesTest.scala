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
}
