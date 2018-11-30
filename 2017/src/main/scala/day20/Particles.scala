package day20

import scala.collection.immutable.Stream.iterate
import scala.math.abs
import scala.util.parsing.combinator.RegexParsers

object Particles {

  def survived(text: String): Int =
    iterate(ParticleParser(text))(ps => filterCollided(ps.map(after(1, _))))
      .drop(1000)
      .head
      .size

  def closest(text: String): Int =
    ParticleParser(text)
      .map(after(1000, _))
      .zipWithIndex
      .minBy(p => distance(p._1.p))
      ._2

  def filterCollided(particles: Seq[Particle]): Seq[Particle] =
    particles.groupBy(_.p).filter(_._2.size == 1).values.flatten.toSeq

  def after(time: Long, par: Particle): Particle = {
    Particle(
      (pos(time, par.p._1, par.v._1, par.a._1),
       pos(time, par.p._2, par.v._2, par.a._2),
       pos(time, par.p._3, par.v._3, par.a._3)),
      (speed(time, par.v._1, par.a._1),
       speed(time, par.v._2, par.a._2),
       speed(time, par.v._3, par.a._3)),
      par.a
    )
  }

  private def distance(v: V): Long = abs(v._1) + abs(v._2) + abs(v._3)

  private def pos(time: Long, p: Long, v: Long, a: Long): Long =
    p + time * v + time * time * a

  private def speed(time: Long, v: Long, a: Long): Long = v + a * time

  type V = (Long, Long, Long)

  case class Particle(p: V, v: V, a: V)

  object ParticleParser extends RegexParsers {

    def particle: Parser[Particle] =
      ("p=" ~> vector <~ ", ") ~ ("v=" ~> vector <~ ", ") ~ ("a=" ~> vector) ^^ {
        case p ~ v ~ a => Particle(p, v, a)
      }

    def vector: Parser[V] =
      ("<" ~> amount) ~ ("," ~> amount) ~ (("," ~> amount) <~ ">") ^^ {
        case a1 ~ a2 ~ a3 => (a1, a2, a3)
      }

    def amount: Parser[Int] = """-?\d+""".r ^^ (_.toInt)

    def apply(text: String): Seq[Particle] =
      parseAll(rep(particle), text) match {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.toString)
      }
  }

}
