package day22

import day22.Virus.{Down, Left, Position, Right, Up, dumbBurst, infected}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class VirusTest extends FlatSpec {
  "Virus" should "turn right when cell is clean" in {
    assert(dumbBurst(Set(), Position(direction = Up))._2.direction == Left)
    assert(dumbBurst(Set(), Position(direction = Down))._2.direction == Right)
  }
  it should "turn left when cell is infected" in {
    assert(dumbBurst(Set((0, 0)), Position(direction = Up))._2.direction == Right)
    assert(dumbBurst(Set((0, 0)), Position(direction = Down))._2.direction == Left)
  }
  it should "clean infected cell" in {
    assert(dumbBurst(Set((0, 0), (1, 1)), Position())._1 == Set((1, 1)))
  }
  it should "infect clean cell" in {
    assert(dumbBurst(Set((1, 1)), Position())._1 == Set((0, 0), (1, 1)))
  }
  it should "move forward anyway" in {
    assert(dumbBurst(Set(), Position(direction = Up))._2.pos == (-1, 0))
    assert(dumbBurst(Set(), Position(direction = Down))._2.pos == (1, 0))
    assert(dumbBurst(Set((0, 0)), Position(direction = Up))._2.pos == (1, 0))
    assert(dumbBurst(Set((0, 0)), Position(direction = Down))._2.pos == (-1, 0))
  }

  "Infected" should "count number of infection during N bursts" in {
    assert(infected(7, Set((-1, 0), (1, -1)), Position()) == 5)
    assert(infected(70, Set((-1, 0), (1, -1)), Position()) == 41)
    assert(infected(10000, Set((-1, 0), (1, -1)), Position()) == 5587)
  }
  it should "solve my input" in {
    assert(infected(10000, myInput, Position()) == 5552)
  }

  private val myInput =
    fromURL(getClass.getResource("/day22/input.txt"))
      .getLines()
      .zipWithIndex
      .flatMap {
        case (line, y) =>
          line.zipWithIndex.flatMap {
            case (c, x) => if (c == '#') Seq((x, y)) else Seq()
          }
      }
      .map { case (x, y) => (x - 12, y - 12) }
      .toSet
}
