package day03

import day03.SpiralMemory._
import org.scalatest.FlatSpec

class SpiralMemoryTest extends FlatSpec {
  "First spiral turn" should "have single item" in {
    assert(turnOf(1).index == 0)
  }
  "Second spiral turn" should "have 8 items" in {
    2 to 9 foreach (position => assert(turnOf(position).index == 1))
  }
  "Third spiral turn" should "have 16 items" in {
    10 to 25 foreach (position => assert(turnOf(position).index == 2))
  }

  "To grid and back" should "preserve position" in {
    (1 to 1000).foreach(position =>
      assert(fromGrid(toGrid(position)) == position))
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
    assert(fibonachiAt(1) == 1)
    assert(fibonachiAt(2) == 1)
    assert(fibonachiAt(3) == 2)
    assert(fibonachiAt(10) == 26)
    assert(fibonachiAt(23) == 806)
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
