package day07

import day07.RecursiveCircus.{balancedWeight, root}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class RecursiveCircusTest extends FlatSpec {

  "Root" should "find the name of the bottom program" in {
    assert(
      root(
        Seq(
          ("a" -> 0) -> Seq("b", "c"),
          ("b" -> 0) -> Seq("d", "e"),
          ("c" -> 0) -> Seq(),
          ("d" -> 0) -> Seq(),
          ("e" -> 0) -> Seq()
        )) == "a")
  }
  it should "satisfy example" in {
    assert(root(example) == "tknk")
  }
  it should "solve my input" in {
    assert(root(myInput) == "qibuqqg")
  }

  "Balanced weight" should "be weight of unbalanced programm to make it balanced" in {
    assert(balancedWeight(Seq(
      ("a" -> 1) -> Seq("aa", "ab", "ac"),
      ("aa" -> 3) -> Seq("aaa", "aab", "aac"),
      ("ab" -> 9) -> Seq(),
      ("ac" -> 9) -> Seq(),
      ("aaa" -> 1) -> Seq(), //unbalanced
      ("aab" -> 2) -> Seq(),
      ("aac" -> 2) -> Seq()
    )) == 2)
  }
  it should "satisfy example" in {
    assert(balancedWeight(example) == 60)
  }
  it should "solve my input" in {
    assert(balancedWeight(myInput) == 1079)
  }

  private def example =
    """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)""".stripMargin.split("\n").map(parseLine)

  private def myInput =
    fromURL(getClass.getResource("/day07/input.txt"))
      .getLines()
      .map(parseLine)
      .toSeq

  private def parseLine(line: String): ((String, Int), Seq[String]) = {
    val regex = """(.*) \((\d*)\)( -> )?(.+)?""".r("name", "weight", "sep", "subNames")
    val matches = regex.findFirstMatchIn(line).get
    val subs = Option(matches.group("subNames")).fold(Seq[String]())(names => names.split(", ").toSeq)
    val weight = matches.group("weight").toInt
    ((matches.group("name"), weight), subs)
  }
}
