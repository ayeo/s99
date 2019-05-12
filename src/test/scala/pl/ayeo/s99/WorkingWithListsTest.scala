package pl.ayeo.s99

import org.scalatest.{FunSuite}

//todo: add missing tests for 2-8
class WorkingWithListsTest extends FunSuite {
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
    assert(
      processor.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
        List('a, 'b, 'c, 'a, 'd, 'e)
    )
  }

  //P09
  test("pack with example data") {
    assert(
      processor.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
        List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    )
  }

  //P10
  test("encode with example data") {
    assert(
      processor.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    )
  }

  //P11
  test("encodeModified with example data") {
    assert(
      processor.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
        List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
    )
  }

  //P12
  test("decode with example data") {
    assert(
      processor.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ===
        List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    )
  }

  //P13
  test("decodeDirect with example data") {
    assert(
      processor.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
        List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    )
  }

  //P14
  test("duplicate with example data") {
    assert(
      processor.duplicate(List('a, 'b, 'c, 'c, 'd)) ===
        List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
    )
  }

  //P15
  test("duplicateN with example data") {
    assert(
      processor.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) ===
        List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    )
  }

  //P16
  test("drop with example data") {
    assert(
      processor.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
        List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
    )
  }

  //P17
  test("split with example data") {
    assert(
      processor.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
        List(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) //fixme: should return tuple
    )
  }

  //P18
  test("slice with example data") {
    assert(
      processor.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
        List('d, 'e, 'f, 'g)
    )
  }

  //P19
  test("rotate with example data") {
    assert(
      processor.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
        List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    )
    assert(
      processor.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
        List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    )
  }

  //P20
  test("removeAt with example data") {
    assert(
      processor.removeAt(1, List('a, 'b, 'c, 'd)) ===
        (List('a, 'c, 'd), 'b)
    )
  }

  //P21
  test("insertAt with example data") {
    assert(
      processor.insertAt('new, 1, List('a, 'b, 'c, 'd)) ===
        List('a, 'new, 'b, 'c, 'd)
    )
  }

  //P22
  test("range with example data") {
    assert(
      processor.range(4, 9) ===
        List(4, 5, 6, 7, 8, 9)
    )
  }

  //P23
  test("randomSelect with example data") {
    assert(processor.randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)).length === 3)
  }

  //P24
  test("lotto with example data") {
    assert(processor.lotto(6, 49).length === 6)
  }

  //P25
  test("randomPermute with example data") {
    assert(processor.randomPermute(List('b, 'a, 'd, 'c, 'e, 'f)) === 6)
  }
}
