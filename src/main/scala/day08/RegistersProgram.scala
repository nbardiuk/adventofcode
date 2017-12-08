package day08

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

    val newValue = instruction.operation match {
      case Inc(amount) => memory(instruction.register) + amount
      case Dec(amount) => memory(instruction.register) - amount
    }

    if (matchesCondition) memory.updated(instruction.register, newValue)
    else memory
  }

  def initMemory(instructions: Seq[Instruction]): Map[String, Int] =
    instructions
      .flatMap(instr => Seq((instr.register, 0), (instr.condition.register, 0)))
      .toMap

  def instruction(text: String): Instruction = {
    val matched =
      """([^ ]*) ([^ ]*) (-?\d+) if ([^ ]*) ([^ ]*) (-?\d+)"""
        .r("register",
           "operation",
           "amount",
           "condition.register",
           "condition",
           "condition.amount")
        .findFirstMatchIn(text)
        .get
    val register = matched.group("register")

    val amount = matched.group("amount").toInt
    val operation = matched.group("operation") match {
      case "inc" => Inc(amount)
      case "dec" => Dec(amount)
    }

    val conr = matched.group("condition.register")
    val cona = matched.group("condition.amount").toInt
    val condition = matched.group("condition") match {
      case ">"  => GT(conr, cona)
      case "<"  => LT(conr, cona)
      case "<=" => LTE(conr, cona)
      case ">=" => GTE(conr, cona)
      case "==" => Eq(conr, cona)
      case "!=" => NE(conr, cona)
    }
    Instruction(register, operation, condition)
  }

  sealed trait Operation
  case class Inc(amount: Int) extends Operation
  case class Dec(amount: Int) extends Operation

  sealed trait Condition {
    def register: String
  }
  case class GT(register: String, amount: Int) extends Condition
  case class LT(register: String, amount: Int) extends Condition
  case class GTE(register: String, amount: Int) extends Condition
  case class LTE(register: String, amount: Int) extends Condition
  case class Eq(register: String, amount: Int) extends Condition
  case class NE(register: String, amount: Int) extends Condition

  case class Instruction(register: String,
                         operation: Operation,
                         condition: Condition)

}
