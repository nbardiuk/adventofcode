package day08

import day08.RegistersProgram._
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class RegistersProgramTest extends FlatSpec {
  "Instruction register" should "should be in the beginning" in {
    assert(instruction("b inc 5 if a > 1").operation.register == "b")
  }
  it can "have multiple letters" in {
    assert(instruction("asff inc 5 if fdsa == 1").operation.register == "asff")
    assert(instruction("asff inc 5 if fdsa == 1").condition.register == "fdsa")
  }

  "Instruction operation" can "be increment" in {
    assert(instruction("b inc 5 if a > 1").operation == Inc("b", 5))
  }
  it can "have decrement" in {
    assert(instruction("b dec 2 if a > 1").operation == Dec("b", 2))
  }
  it can "have negative value in operation" in {
    assert(instruction("b inc -2 if a > 1").operation == Inc("b", -2))
  }

  "Instruction condition" can "be equality" in {
    assert(instruction("c inc -20 if c == 10").condition == Eq("c", 10))
  }
  it can "be >" in {
    assert(instruction("c inc -20 if c > -1").condition == GT("c", -1))
  }
  it can "be <" in {
    assert(instruction("c inc -20 if c < -1").condition == LT("c", -1))
  }
  it can "be >=" in {
    assert(instruction("c inc -20 if c >= -1").condition == GTE("c", -1))
  }
  it can "be <=" in {
    assert(instruction("c inc -20 if c <= -1").condition == LTE("c", -1))
  }
  it can "be !=" in {
    assert(instruction("c inc -20 if c != -1").condition == NE("c", -1))
  }

  "Calculation" should "start with 0 in registers" in {
    assert(
      calculate(
        Seq(
          instruction("b inc 0 if a > 10000"),
          instruction("c inc 0 if d > 10000")
        )) == Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0))
  }
  it should "chage register state when condition matches" in {
    assert(
      calculate(
        Seq(
          instruction("b inc 1 if a == 0"),
          instruction("c inc 2 if b == 1")
        )) == Map("a" -> 0, "b" -> 1, "c" -> 2))
  }

  "Max final memory" should "return maximum value of any register after calculation" in {
    assert(
      maxFinalMemory(
        Seq(
          instruction("b inc 1 if a == 0"),
          instruction("c inc 2 if b == 1")
        )) == 2)
  }
  it should "match example" in {
    assert(maxFinalMemory(example) == 1)
  }
  it should "solve my input " in {
    assert(maxFinalMemory(myInput) == 6061)
  }

  "Max runtime memory" should "return maximum value of any register during calculation" in {
    assert(
      maxRuntimeMemory(
        Seq(
          instruction("b inc 100 if a == 0"),
          instruction("b dec 95 if a == 0")
        )) == 100)
  }
  it should "match example" in {
    assert(maxRuntimeMemory(example) == 10)
  }
  it should "solve my input " in {
    assert(maxRuntimeMemory(myInput) == 6696)
  }

  private def example =
    """b inc 5 if a > 1
      |a inc 1 if b < 5
      |c dec -10 if a >= 1
      |c inc -20 if c == 10""".stripMargin.lines.map(instruction).toSeq

  private def myInput =
    fromURL(getClass.getResource("/day08/input.txt"))
      .getLines()
      .map(instruction)
      .toSeq

}
