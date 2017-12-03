package day03

import day03.SpiralMemory._
import org.scalatest.FlatSpec

class SpiralMemoryTest extends FlatSpec {
  "First spiral" should "have single item" in {
    assert(turnOf(1).index == 0)
  }
  "Second spiral" should "have 8 items" in {
    assert(turnOf(2).index == 1)
    assert(turnOf(9).index == 1)
  }
  "Third spiral" should "have 16 items" in {
    assert(turnOf(10).index == 2)
    assert(turnOf(25).index == 2)
  }

  "Grid coordinate" should "" in {
    assert(toGrid(1) == (0, 0))
    assert(toGrid(2) == (1, 0))
    assert(toGrid(3) == (1, 1))
    assert(toGrid(4) == (0, 1))
    assert(toGrid(5) == (-1, 1))
    assert(toGrid(6) == (-1, 0))
    assert(toGrid(7) == (-1, -1))
    assert(toGrid(8) == (0, -1))
    assert(toGrid(9) == (1, -1))
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

  "from grid coordinate" should "" in {
    assert(fromGrid(0, 0) == 1)
    assert(fromGrid(1, 0) == 2)
    assert(fromGrid(1, 1) == 3)
    assert(fromGrid(0, 1) == 4)
    assert(fromGrid(-1, 1) == 5)
    assert(fromGrid(-1, 0) == 6)
    assert(fromGrid(-1, -1) == 7)
    assert(fromGrid(0, -1) == 8)
    assert(fromGrid(1, -1) == 9)
  }

  "to grid and back" should "preserve" in {
    (1 to 1000).foreach(i => assert(fromGrid(toGrid(i)) == i))
  }

  "grid fibonachi" should "" in {
    assert(gridFibonachi(1) == 1)
    assert(gridFibonachi(2) == 1)
    assert(gridFibonachi(3) == 2)
    assert(gridFibonachi(10) == 26)
    assert(gridFibonachi(23) == 806)
  }

  "first grid fibonachi after" should "" in {
    assert(firstFibonachiAfter(3) == 4)
    assert(firstFibonachiAfter(60) == 122)
  }
  it should "calculate my input" in {
    assert(firstFibonachiAfter(325489) == 330785)
  }
}
