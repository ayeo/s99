package pl.ayeo.s99

import org.scalatest.{BeforeAndAfterAll, FunSuite}

class WorkingWithListsTest extends FunSuite with BeforeAndAfterAll {
  private val processor: WorkingWithLists = WorkingWithLists()

  test("last with example data") {
    assert(processor.last(List(1, 1, 2, 3, 5, 8)) === 8)
    assert(processor.last(List(1, 1, 2, 3)) === 3)
  }

  test("last with one element") {
    assert(processor.last(List(2)) === 2)
  }

  test("last with empty data") {
    intercept[IllegalArgumentException] {
      processor.last(List())
    }
  }

  test("penultimate with example data") {
    assert(processor.penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
    assert(processor.penultimate(List(2, 3, 5, 8, 7, 0)) === 7)
    assert(processor.penultimate(List(2, 3, 5)) === 3)
  }


  test("penultimate with empty list") {
    intercept[IllegalArgumentException] {
      processor.penultimate(List())
    }
  }

  test("penultimate with one element") {
    intercept[IllegalArgumentException] {
      processor.penultimate(List(2))
    }
  }

  test("penultimate with two elements data") {
    assert(processor.penultimate(List(3, 5)) === 3)
  }

  //P08
  test("compress with example data") {
    assert(processor.compress(
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List('a, 'b, 'c, 'a, 'd, 'e)
    )
  }

  //P09
  test("pack with example data") {
    assert(processor.pack(
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    )
  }

  //P10
  test("encode with example data") {
    assert(processor.encode(
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    )
  }
}
