package day03

import day03.SpiralMemory.{distance, distanceToCross, spiralOf}
import org.scalatest.FlatSpec

class SpiralMemoryTest extends FlatSpec {
  "First spiral" should "have single item" in {
    assert(spiralOf(1).index == 0)
  }
  "Second spiral" should "have 8 items" in {
    assert(spiralOf(2).index == 1)
    assert(spiralOf(9).index == 1)
  }
  "Third spiral" should "have 16 items" in {
    assert(spiralOf(10).index == 2)
    assert(spiralOf(25).index == 2)
  }

  "First spiral cross" should "be" in {
    assert(distanceToCross(1) == 0)
  }
  "Second spiral cross" should "be" in {
    assert(distanceToCross(2) == 0)
    assert(distanceToCross(4) == 0)
    assert(distanceToCross(6) == 0)
    assert(distanceToCross(8) == 0)
  }
  "Third spiral cross" should "be" in {
    assert(distanceToCross(11) == 0)
    assert(distanceToCross(15) == 0)
    assert(distanceToCross(19) == 0)
    assert(distanceToCross(23) == 0)
  }

  "Distance" should "match example data" in {
    assert(distance(1) == 0)
    assert(distance(12) == 3)
    assert(distance(23) == 2)
    assert(distance(1024) == 31)
  }

  it should "calculate my data" in {
    assert(distance(325489) == 552)
  }
}
