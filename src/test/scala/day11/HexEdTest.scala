package day11

import day11.HexEd.{distance, maxDistance}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class HexEdTest extends FlatSpec {

  "Distance" should "be number of cells from the start" in {
    assert(distance("n", "n", "n") == 3)
  }
  it should "not count coming back" in {
    assert(distance("n", "s") == 0)
    assert(distance("nw", "se") == 0)
    assert(distance("ne", "sw") == 0)
  }
  it should "consider shorter path in the middle" in {
    assert(distance("nw", "ne") == 1)
    assert(distance("n", "se") == 1)
    assert(distance("ne", "s") == 1)
    assert(distance("se", "sw") == 1)
    assert(distance("s", "nw") == 1)
  }
  it should "satisfy task example" in {
    assert(distance("ne", "ne", "ne") == 3)
    assert(distance("ne", "ne", "sw", "sw") == 0)
    assert(distance("ne", "ne", "s", "s") == 2)
    assert(distance("se", "sw", "se", "sw", "sw") == 3)
  }
  it should "solve my input" in {
    assert(distance(myInput: _*) == 715)
  }

  "Max distance" should "be the biggest distance through the path" in {
    assert(maxDistance("n", "n", "n", "s", "s", "s") == 3)
  }
  it should "solve my input" in {
    assert(maxDistance(myInput: _*) == 1512)
  }

  private def myInput =
    fromURL(getClass.getResource("/day11/input.txt")).getLines().mkString.split(",")
}
