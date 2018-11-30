package day19

import day19.Tubes.{pathLength, pathText}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class TubesTest extends FlatSpec {

  "Path text" should "read letters on the path" in {
    assert(pathText(
      """     |
        >     |  +--+
        >     A  |  C
        > F---|----E|--+
        >     |  |  |  D
        >     +B-+  +--+ """.stripMargin('>')) == "ABCDEF")
  }
  it should "solve my input" in {
    assert(pathText(myInput) == "EPYDUXANIT")
  }

  "Path length" should "number of steps on the path" in {
    assert(pathLength(
      """     |
        >     |  +--+
        >     A  |  C
        > F---|----E|--+
        >     |  |  |  D
        >     +B-+  +--+ """.stripMargin('>')) == 38)
  }
  it should "solve my input" in {
    assert(pathLength(myInput) == 17544)
  }

  private val myInput = fromURL(getClass.getResource("/day19/input.txt")).getLines().mkString("\n")
}
