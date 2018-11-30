package day18

import scala.annotation.tailrec
import scala.collection.immutable.Stream.iterate
import scala.util.parsing.combinator.RegexParsers

object Duet {

  def firstRcv(commands: String): Long = {
    val operations: Seq[Operation] = OperationParser(commands)

    @tailrec
    def loop(i: Int, mem: Map[String, Long], lastSnd: Long): Long = operations(i) match {
      case Set(r, s) => loop(i + 1, mem.updated(r, read(s, mem)), lastSnd)
      case Add(r, s) => loop(i + 1, mem.updated(r, read(r, mem) + read(s, mem)), lastSnd)
      case Mul(r, s) => loop(i + 1, mem.updated(r, read(r, mem) * read(s, mem)), lastSnd)
      case Mod(r, s) => loop(i + 1, mem.updated(r, read(r, mem) % read(s, mem)), lastSnd)
      case Jgz(r, s) => loop(i + (if (read(r, mem) <= 0) 1 else read(s, mem).toInt), mem, lastSnd)
      case Snd(s) => loop(i + 1, mem, read(s, mem))
      case Rcv(r) => if (read(r, mem) == 0) loop(i + 1, mem, lastSnd) else lastSnd
    }

    loop(0, Map(), 0)
  }


  def secondSent(commands: String): Long = {
    val operations: Seq[Operation] = OperationParser(commands)

    def runUntilBlocked(p: Program, inbox: Seq[Long]): (Program, Seq[Long]) = {
      @tailrec
      def loop(i: Int, mem: Map[String, Long], inbox: Seq[Long], outbox: Seq[Long]): (Program, Seq[Long]) = operations(i) match {
        case Set(r, s) => loop(i + 1, mem.updated(r, read(s, mem)), inbox, outbox)
        case Add(r, s) => loop(i + 1, mem.updated(r, read(r, mem) + read(s, mem)), inbox, outbox)
        case Mul(r, s) => loop(i + 1, mem.updated(r, read(r, mem) * read(s, mem)), inbox, outbox)
        case Mod(r, s) => loop(i + 1, mem.updated(r, read(r, mem) % read(s, mem)), inbox, outbox)
        case Jgz(c, s) => loop(i + (if (read(c, mem) <= 0) 1 else read(s, mem).toInt), mem, inbox, outbox)
        case Snd(s) => loop(i + 1, mem, inbox, outbox :+ read(s, mem))
        case Rcv(r) => if (inbox.isEmpty) (Program(mem, i), outbox)
        else loop(i + 1, mem.updated(r, inbox.head), inbox.tail, outbox)
      }

      loop(p.i, p.mem, inbox, Vector())
    }

    def untilBlocked(duet: Duet):Duet = {
      val (p0, q1) = runUntilBlocked(duet.p0, duet.q0)
      val (p1, q0) = runUntilBlocked(duet.p1, duet.q1)
      Duet(p0, q0, p1, q1)
    }

    def notDeadlock(duet: Duet):Boolean = duet.q0.nonEmpty || duet.q1.nonEmpty

    iterate(Duet())(untilBlocked).drop(1).takeWhile(notDeadlock).map(_.q0.size).sum
  }

  case class Duet(p0: Program = Program(), q0: Seq[Long] = Seq(), p1: Program = Program(Map("p" -> 1)), q1: Seq[Long] = Seq())
  case class Program(mem: Map[String, Long] = Map(), i: Int = 0)

  def read(source: Source, memory: Map[String, Long]): Long =
    source match {
      case Value(value) => value
      case Register(name) => read(name, memory)
    }

  def read(register: String, memory: Map[String, Long]): Long = memory.getOrElse(register, 0)

  object OperationParser extends RegexParsers {

    def operations: Parser[Seq[Operation]] = rep(operation)

    def operation: Parser[Operation] =
      pair("set", register, source)(Set) |
        pair("add", register, source)(Add) |
        pair("mul", register, source)(Mul) |
        pair("mod", register, source)(Mod) |
        pair("jgz", source, source)(Jgz) |
        "snd" ~> source ^^ Snd |
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
  case class Jgz(condition: Source, distance: Source) extends Operation
  case class Snd(source: Source) extends Operation
  case class Rcv(register: String) extends Operation

  sealed trait Source
  case class Register(name: String) extends Source
  case class Value(value: Int) extends Source

}
