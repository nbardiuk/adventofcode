package day04

import day04.Passphrase.{
  countUnique,
  countWithoutAnagrams,
  unique,
  withoutAnagrams
}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class PassphraseTest extends FlatSpec {

  "Passphrase with unique words" should "be valid" in {
    assert(unique("aa bb cc dd"))
    assert(unique("aa aaa"))
  }
  "Passphrase with duplicate words" should "be invalid" in {
    assert(!unique("aa aa"))
  }

  "Count unique" should "count valid passphrases" in {
    assert(countUnique(Seq("aa aaa", "a aa", "aa aa")) == 2)
  }
  it should "count my input" in {
    assert(countUnique(myInput) == 337)
  }

  "Passphrase without anagrams" should "be valid" in {
    assert(withoutAnagrams("abc def"))
    assert(withoutAnagrams("a ab abc abd abf abj"))
    assert(withoutAnagrams("iiii oiii ooii oooi oooo"))
  }
  "Passphrase with anagrams" should "be invalid" in {
    assert(!withoutAnagrams("abc cba"))
  }

  "Count without anagrams" should "count passphrases without anagrams" in {
    assert(countWithoutAnagrams(Seq("aa aaa", "ab aab", "ab ba")) == 2)
  }
  it should "count my input" in {
    assert(countWithoutAnagrams(myInput) == 231)
  }

  private lazy val myInput: Seq[String] =
    fromURL(getClass.getResource("/day04/input")).getLines().toSeq

}
