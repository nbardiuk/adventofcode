package day09

import day09.StreamProcessing.{garbageSize, groupScore}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class StreamProcessingTest extends FlatSpec {

  "Group score" should "be 1 for single group" in {
    assert(groupScore("{}") == 1)
  }
  it should "be sum of group scores" in {
    assert(groupScore("{}{}") == 2)
  }
  it should "be 2 for nested group" in {
    assert(groupScore("{{}}") == 3)
  }
  it should "be 3 for third nested group" in {
    assert(groupScore("{{{}}}") == 6)
  }
  it should "not count garbage group" in {
    assert(groupScore("{<{ads}fd{sdf}>}") == 1)
  }
  it should "count group with garbage" in {
    assert(groupScore("{{<a>}{<a>}}") == 5)
  }
  it should "ignore deleted garbage symbol" in {
    assert(groupScore("{{<a!>}{<a!>}{<a>}}") == 3)
  }
  it should "ignore deleted delete symbol" in {
    assert(groupScore("{{<!!>}{<!!>}{<a>}}") == 7)
  }
  it should "solve my input" in {
    assert(groupScore(myInput) == 14421)
  }

  "Garbage size" should "count number garbage characters" in {
    assert(garbageSize("<123456>") == 6)
  }
  it should "not count canceled characters" in {
    assert(garbageSize("<!<1!>>") == 1)
  }
  it should "solve my input" in {
    assert(garbageSize(myInput) == 6817)
  }

  private def myInput =
    fromURL(getClass.getResource("/day09/input.txt")).getLines().next()
}
