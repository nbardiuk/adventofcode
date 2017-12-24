package day21

import day21.Fractal._
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class FractalTest extends FlatSpec {

  "Enhancement" should "replace all rotations of 3x3 key" in {
    val rotated0 = toImage(
      """.#.
        |..#
        |###""".stripMargin)
    val rotated90 = toImage(
      """#..
        |#.#
        |##.""".stripMargin)
    val rotated180 = toImage(
      """###
        |#..
        |.#.""".stripMargin)
    val rotated270 = toImage(
      """.##
        |#.#
        |..#""".stripMargin)

    val rules = readRules(Seq(".#./..#/### => #..#/..../..../#..#"))

    val enhanced = toImage(
      """#..#
        |....
        |....
        |#..#""".stripMargin)

    assert(enhance(rotated0, rules) == enhanced)
    assert(enhance(rotated90, rules) == enhanced)
    assert(enhance(rotated180, rules) == enhanced)
    assert(enhance(rotated270, rules) == enhanced)
  }

  it should "replace all flips of rotations of 3x3 key" in {
    val flipped0 = toImage(
      """.#.
        |#..
        |###""".stripMargin)
    val flipped90 = toImage(
      """..#
        |#.#
        |.##""".stripMargin)
    val flipped180 = toImage(
      """###
        |..#
        |.#.""".stripMargin)
    val flipped270 = toImage(
      """##.
        |#.#
        |#..""".stripMargin)

    val rules = readRules(Seq(".#./..#/### => #..#/..../..../#..#"))

    val enhanced = toImage(
      """#..#
        |....
        |....
        |#..#""".stripMargin)

    assert(enhance(flipped0, rules) == enhanced)
    assert(enhance(flipped90, rules) == enhanced)
    assert(enhance(flipped180, rules) == enhanced)
    assert(enhance(flipped270, rules) == enhanced)
  }

  "Expand" should "split image into squares and expand using enhancements" in {
    val image = toImage(
      """#..#
        |....
        |....
        |#..#""".stripMargin)
    val rules = readRules(Seq("../.# => ##./#../..."))
    val expanded = toImage(
      """##.##.
        |#..#..
        |......
        |##.##.
        |#..#..
        |......""".stripMargin)

    assert(expand(image, rules) == expanded)
  }

  "Count On" should "be number of enabled pixels after n iterations" in {
    val image = toImage(
      """.#.
        |..#
        |###""".stripMargin)
    val rules = readRules(
      Seq("../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"))

    assert(countOn(image, rules, 2) == 12)
  }
  it should "solve my input 5 iterations" in {
    val image = toImage(
      """.#.
        |..#
        |###""".stripMargin)
    assert(countOn(image, myInput, 5) == 184)
  }
  it should "solve my input 18 iterations" in {
    val image = toImage(
      """.#.
        |..#
        |###""".stripMargin)
    assert(countOn(image, myInput, 18) == 2810258)
  }
  private val myInput = readRules(
    fromURL(getClass.getResource("/day21/input.txt")).getLines().toSeq)
}
