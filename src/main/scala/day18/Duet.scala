package day18

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Duet {

  def firstSound(commands: String): Long = {
    val operations: Seq[Operation] = OperationParser(commands)

    @tailrec
    def loop(index: Int, memory: Map[String, Long], lastSound: Long): Long = {
      operations(index) match {
      case Set(r, s) => loop(index + 1, memory.updated(r, readSource(s, memory)), lastSound)
      case Add(r, s) => loop(index + 1, memory.updated(r, readMemory(r, memory) + readSource(s, memory)), lastSound)
      case Mul(r, s) => loop(index + 1, memory.updated(r, readMemory(r, memory) * readSource(s, memory)), lastSound)
      case Mod(r, s) => loop(index + 1, memory.updated(r, readMemory(r, memory) % readSource(s, memory)), lastSound)
      case Jgz(r, s) => loop(if (readSource(r, memory) == 0) index + 1 else index + readSource(s, memory).toInt, memory, lastSound)
      case Snd(r) => loop(index + 1, memory, readMemory(r, memory))
      case Rcv(r) => if (readMemory(r, memory) == 0) loop(index + 1, memory, lastSound) else lastSound
    }}

    loop(0, Map(), 0)
  }

  def readSource(source: Source, memory: Map[String, Long]): Long =
    source match {
      case Value(value) => value
      case Register(name) => readMemory(name, memory)
    }
  def readMemory(register: String, memory: Map[String, Long]): Long = memory.getOrElse(register, 0)

  object OperationParser extends RegexParsers {

    def operations: Parser[Seq[Operation]] = rep(operation)

    def operation: Parser[Operation] =
      pair("set", register, source)(Set) |
        pair("add", register, source)(Add) |
        pair("mul", register, source)(Mul) |
        pair("mod", register, source)(Mod) |
        pair("jgz", source, source)(Jgz) |
        "snd" ~> register ^^ Snd |
        "rcv" ~> register ^^ Rcv

    def source: Parser[Source] = register ^^ Register | amount ^^ Value

    def register: Parser[String] = """[a-z]+""".r ^^ (_.toString)

    def amount: Parser[Int] = """-?\d+""".r ^^ (_.toInt)

    def apply(text: String): Seq[Operation] = parseAll(operations, text) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.toString)
    }

    def pair[U, A, B](command: String, a: Parser[A], b: Parser[B])(
      join: (A, B) => U): Parser[U] = command ~> a ~ b ^^ {
      case a ~ b => join(a, b)
    }
  }

  sealed trait Operation

  case class Set(register: String, source: Source) extends Operation

  case class Add(register: String, source: Source) extends Operation

  case class Mul(register: String, source: Source) extends Operation

  case class Mod(register: String, source: Source) extends Operation

  case class Jgz(register: Source, source: Source) extends Operation

  case class Snd(register: String) extends Operation

  case class Rcv(register: String) extends Operation

  sealed trait Source

  case class Register(name: String) extends Source

  case class Value(value: Int) extends Source

}
