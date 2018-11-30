package day16

import scala.collection.immutable.Stream.iterate
import scala.util.parsing.combinator.RegexParsers

object Permutation {

  def danceTimes(times: Int, moves: String, programs: String): String = {
    val dances = iterate(programs)(dance(moves)(_))
    val cycle = dances.indexOf(programs, 1)
    dances.drop(times % cycle).head
  }

  def dance(moves: String): Dance = compose(Parser(moves))

  private def compose(dances: Seq[Dance]): Dance =
    dances.foldLeft(_)((s, d) => d(s))

  type Dance = String => String
  def spin(length: Int): Dance = s => s.takeRight(length) ++ s.dropRight(length)
  def swap(a: Int, b: Int): Dance = s => s.updated(a, s(b)).updated(b, s(a))
  def swap(a: Char, b: Char): Dance = s => swap(s.indexOf(a), s.indexOf(b))(s)

  object Parser extends RegexParsers {
    def instructions = repsep(instruction, ",")
    def instruction = sp | ex | prt

    def sp = "s" ~> int ^^ spin
    def ex = ("x" ~> int) ~ ("/" ~> int) ^^ {
      case a ~ b => swap(a, b)
    }
    def prt = ("p" ~> char) ~ ("/" ~> char) ^^ {
      case a ~ b => swap(a, b)
    }

    def char = """[a-z]""".r ^^ (_.charAt(0))
    def int = """\d+""".r ^^ (_.toInt)

    def apply(text: String): Seq[Dance] =
      parseAll(instructions, text) match {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
      }
  }
}
