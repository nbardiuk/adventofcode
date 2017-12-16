package day15

import day15.DuelingGenerators.{matches, score1, score2}
import org.scalatest.FlatSpec

class DuelingGeneratorsTest extends FlatSpec {

  "Pair" should "match when lower 16 bits are the same" in {
    assert(matches(245556042, 1431495498))
    assert(!matches(1092455, 430625591))
  }

  "Score 1" should "be number of matches in 40 million generated pairs" in {
    assert(score1(65, 8921) == 588)
  }
  it should "solve my input" in {
    assert(score1(722, 354) == 612)
  }

  "Score 2" should "be number of matches in 5 million generated pairs with filter" in {
    assert(score2(65, 8921) == 309)
  }
  it should "solve my input" in {
    assert(score2(722, 354) == 285)
  }

}
