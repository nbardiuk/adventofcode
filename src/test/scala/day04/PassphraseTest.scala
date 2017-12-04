package day04

import day04.Passphrase.{countValid, valid}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class PassphraseTest extends FlatSpec {

  "Passphrase with unique words" should "be valid" in {
    assert(valid("aa bb cc dd"))
    assert(valid("aa aaa"))
  }
  "Passphrase with duplicate words" should "be invalid" in {
    assert(!valid("aa aa"))
  }

  "Count valid" should "count valid passphrases" in {
    assert(countValid(Seq("aa aaa", "a aa", "aa aa")) == 2)
  }
  it should "count my input" in {
    assert(countValid(myInput) == 337)
  }

  private def myInput: Seq[String] =
    fromURL(getClass.getResource("/day04/input"))
      .getLines()
      .toSeq

}
