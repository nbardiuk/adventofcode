package day06

import day06.MemoryReallocation.{loopSize, numberOfReallocations, reallocation}
import org.scalatest.FlatSpec

class MemoryReallocationTest extends FlatSpec {

  "Reallocation" should "reallocate blocks to the next buckets" in {
    assert(reallocation(Seq(3, 0, 0, 0)) == Seq(0, 1, 1, 1))
  }
  it should "cycle over in the end" in {
    assert(reallocation(Seq(9, 0, 0, 0)) == Seq(2, 3, 2, 2))
  }
  it should "start from the largest number" in {
    assert(reallocation(Seq(0, 0, 7, 0)) == Seq(2, 2, 1, 2))
  }
  it should "start from first of the largest number" in {
    assert(reallocation(Seq(0, 3, 3, 0)) == Seq(1, 0, 4, 1))
  }

  "Size of reallocation cycles" should "count until first repeat" in {
    assert(numberOfReallocations(Seq(1, 0)) == 2)
  }
  it should "satisfy example" in {
    assert(numberOfReallocations(Seq(0, 2, 7, 0)) == 5)
  }
  it should "solve my input" in {
    assert(
      numberOfReallocations(
        Seq(10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6)) == 14029)
  }

  "Reallocation loop size" should "count number of reallocation in a loop" in {
    assert(loopSize(Seq(1, 0)) == 2)
  }
  it should "satisfy example" in {
    assert(loopSize(Seq(0, 2, 7, 0)) == 4)
  }
  it should "solve my input" in {
    assert(
      loopSize(Seq(10, 3, 15, 10, 5, 15, 5, 15, 9, 2, 5, 8, 5, 2, 3, 6)) == 2765)
  }

}
