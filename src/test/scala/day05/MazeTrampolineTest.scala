package day05

import java.lang.Integer.parseInt

import day05.MazeTrampoline.{countJumps, countStrangeJumps, jump, strangeJump}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class MazeTrampolineTest extends FlatSpec {

  "Jump" should "increase current value" in {
    assert(jump(Array(0), 0)._1.toSeq == Seq(1))
  }
  it should "not increase other values" in {
    assert(jump(Array(0, 2), 0)._1.toSeq == Seq(1, 2))
  }
  it should "move cursor forward to the relative position" in {
    assert(jump(Array(1, 1), 0)._1.toSeq == Seq(2, 1))
  }
  it should "move cursor backward to the relative position" in {
    assert(jump(Array(1, -1), 1)._1.toSeq == Seq(1, 0))
  }
  it should "allow to move cursor outside" in {
    assert(jump(Array(2, 0), 0)._2 == 2)
  }

  "Count jumps" should "stop counting when cursor goes outside" in {
    assert(countJumps(Array(1)) == 1)
    assert(countJumps(Array(1, 1)) == 2)
    assert(countJumps(Array(1, -1)) == 3)
  }
  it should "satisfy example" in {
    assert(countJumps(Array(0, 3, 0, 1, -3)) == 5)
  }
  it should "solve my input" in {
    assert(countJumps(myInput) == 343467)
  }

  "Strange jump" should "decrease current value when value >= 3" in {
    assert(strangeJump(Array(3), 0)._1.toSeq == Seq(2))
    assert(strangeJump(Array(4), 0)._1.toSeq == Seq(3))
  }
  it should "increase current value when value < 3" in {
    assert(strangeJump(Array(2), 0)._1.toSeq == Seq(3))
    assert(strangeJump(Array(-2), 0)._1.toSeq == Seq(-1))
  }

  "Count strange jumps" should "solve my input" in {
    assert(countStrangeJumps(myInput) == 24774780)
  }
  it should "satisfy example" in {
    assert(countStrangeJumps(Array(0, 3, 0, 1, -3)) == 10)
  }

  private def myInput: Array[Int] =
    fromURL(getClass.getResource("/day05/input.txt")).getLines().map(parseInt).toArray

}
