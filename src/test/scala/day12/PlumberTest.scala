package day12

import day12.Plumber.{groupSize, groups}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class PlumberTest extends FlatSpec {
  "group size" should "be number of programs that can be connected to program 0" in {
    assert(groupSize("""0 <-> 1, 2
        |1 <-> 0
        |2 <-> 0
        |3 <-> 4
        |4 <-> 3""".stripMargin) == 3)
  }
  it should "count transitive connections" in {
    assert(groupSize("""0 <-> 1
        |1 <-> 0, 2
        |2 <-> 1
        |3 <-> 4
        |4 <-> 3""".stripMargin) == 3)
  }
  it should "satisfy example" in {
    assert(groupSize("""0 <-> 2
        |1 <-> 1
        |2 <-> 0, 3, 4
        |3 <-> 2, 4
        |4 <-> 2, 3, 6
        |5 <-> 6
        |6 <-> 4, 5""".stripMargin) == 6)
  }
  it should "solve my input" in {
    assert(groupSize(myInput) == 152)
  }

  "groups number" should "be number of groups" in {
    assert(groups("""0 <-> 2
        |1 <-> 1
        |2 <-> 0, 3, 4
        |3 <-> 2, 4
        |4 <-> 2, 3, 6
        |5 <-> 6
        |6 <-> 4, 5""".stripMargin) == 2)
  }
  it should "solve my input" in {
    assert(groups(myInput) == 186)
  }

  private def myInput =
    fromURL(getClass.getResource("/day12/input.txt")).getLines().mkString("\n")
}
