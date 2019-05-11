import org.scalatest.{BeforeAndAfterAll, FunSuite}
import pl.ayeo.s99.WorkingWithLists

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
}
