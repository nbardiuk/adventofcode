package day16

import day16.Permutation._
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class PermutationTest extends FlatSpec {

  "Spin" should "move n programs from the end to the beginning" in {
    assert(dance("s1")("abcde") == "eabcd")
    assert(dance("s3")("abcde") == "cdeab")
    assert(dance("s3")("cabed") == "bedca")
  }

  "Exchange" should "swap programs at positions" in {
    assert(dance("x3/4")("abcde") == "abced")
    assert(dance("x0/4")("abcde") == "ebcda")
    assert(dance("x1/3")("cabed") == "cebad")
    assert(dance("x3/4")("eabcd") == "eabdc")
  }

  "Partner" should "swap programs by name" in {
    assert(dance("pe/b")("abcde") == "aecdb")
    assert(dance("pa/e")("cabed") == "cebad")
    assert(dance("pe/b")("eabdc") == "baedc")
  }

  "Dance" should "change programs according to instructions" in {
    assert(dance("s1,x3/4,pe/b")("abcde") == "baedc")
  }
  it should "solve my input" in {
    assert(dance(myInput)("abcdefghijklmnop") == "doeaimlbnpjchfkg")
  }

  "Billion dances" should "dance 1_000_000_000 times" in {
    assert(
      danceTimes(1000000000, myInput, "abcdefghijklmnop") == "agndefjhibklmocp")
  }

  private def myInput =
    fromURL(getClass.getResource("/day16/input.txt")).getLines().mkString("\n")

}
