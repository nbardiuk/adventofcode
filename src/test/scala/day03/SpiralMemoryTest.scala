package day03

import day03.SpiralMemory._
import org.scalatest.FlatSpec

class SpiralMemoryTest extends FlatSpec {

  "To grid and back" should "preserve position" in {
    (1 to 1000).foreach(
      position =>
        assert(fromGrid(toGrid(position)) == position,
               s"positioin:$position, grid:${toGrid(position)}"))
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

  "Fibonachi" should "match example" in {
    assert(fibonachi(1) == 1)
    assert(fibonachi(2) == 1)
    assert(fibonachi(3) == 2)
    assert(fibonachi(10) == 26)
    assert(fibonachi(23) == 806)
  }

  "First fibonachi after value" should "match example" in {
    assert(firstFibonachiAfter(1) == 2)
    assert(firstFibonachiAfter(3) == 4)
    assert(firstFibonachiAfter(60) == 122)
  }
  it should "calculate my input" in {
    assert(firstFibonachiAfter(325489) == 330785)
  }
}
