package day08

import scala.util.parsing.combinator._

object RegistersProgram {

  def maxRuntimeMemory(instructions: Seq[Instruction]): Int =
    trace(instructions).map(maxRegister).max

  def maxFinalMemory(instructions: Seq[Instruction]): Int =
    maxRegister(calculate(instructions))

  private def maxRegister(memory: Map[String, Int]): Int = memory.values.max

  def calculate(instructions: Seq[Instruction]): Map[String, Int] =
    trace(instructions).last

  private def trace(instructions: Seq[Instruction]): Stream[Map[String, Int]] =
    instructions.toStream.scanLeft(initMemory(instructions))(evaluate)

  private def evaluate(memory: Map[String, Int], instruction: Instruction) = {
    val matchesCondition = instruction.condition match {
      case Eq(register, amount)  => memory(register) == amount
      case NE(register, amount)  => memory(register) != amount
      case GT(register, amount)  => memory(register) > amount
      case LT(register, amount)  => memory(register) < amount
      case GTE(register, amount) => memory(register) >= amount
      case LTE(register, amount) => memory(register) <= amount
    }

    def update(register: String)(f: Int => Int) =
      memory.updated(register, f(memory(register)))

    val updatedMemory = () =>
      instruction.operation match {
        case Inc(register, amount) => update(register)(_ + amount)
        case Dec(register, amount) => update(register)(_ - amount)
    }

    if (matchesCondition) updatedMemory() else memory
  }

  def initMemory(instructions: Seq[Instruction]): Map[String, Int] =
    instructions
      .flatMap(i => Seq((i.operation.register, 0), (i.condition.register, 0)))
      .toMap

  def instruction(text: String): Instruction = InstructionParser(text)

  object InstructionParser extends RegexParsers {

    def instruction: Parser[Instruction] =
      pair(operation, "if", condition)(Instruction)

    def operation: Parser[Operation] = inc | dec
    def inc: Parser[Inc] = pair(register, "inc", amount)(Inc)
    def dec: Parser[Dec] = pair(register, "dec", amount)(Dec)

    def condition: Parser[Condition] = eq | ne | gt | lt | gte | lte
    def eq: Parser[Eq] = pair(register, "==", amount)(Eq)
    def ne: Parser[NE] = pair(register, "!=", amount)(NE)
    def gt: Parser[GT] = pair(register, ">", amount)(GT)
    def lt: Parser[LT] = pair(register, "<", amount)(LT)
    def gte: Parser[GTE] = pair(register, ">=", amount)(GTE)
    def lte: Parser[LTE] = pair(register, "<=", amount)(LTE)

    def register: Parser[String] = """[a-z]+""".r ^^ (_.toString)
    def amount: Parser[Int] = """-?\d+""".r ^^ (_.toInt)

    def apply(text: String): Instruction = parseAll(instruction, text) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def pair[U, A, B](a: Parser[A], separator: String, b: Parser[B])(
        join: (A, B) => U): Parser[U] = a ~ separator ~ b ^^ {
      case a ~ _ ~ b => join(a, b)
    }
  }

  sealed trait Operation {
    def register: String
  }

  case class Inc(register: String, amount: Int) extends Operation
  case class Dec(register: String, amount: Int) extends Operation

  sealed trait Condition {
    def register: String
  }
  case class GT(register: String, amount: Int) extends Condition
  case class LT(register: String, amount: Int) extends Condition
  case class GTE(register: String, amount: Int) extends Condition
  case class LTE(register: String, amount: Int) extends Condition
  case class Eq(register: String, amount: Int) extends Condition
  case class NE(register: String, amount: Int) extends Condition

  case class Instruction(operation: Operation, condition: Condition)

}
