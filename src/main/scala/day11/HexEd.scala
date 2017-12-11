package day11

import scala.math.{abs, min}

object HexEd {

  def maxDistance(directions: String*): Int =
    directions.map(toVector).scanLeft(Vector())(_ + _).map(_.size).max

  def distance(directions: String*): Int =
    directions.map(toVector).reduce(_ + _).size

  private def toVector(direction: String) = direction match {
    case "ne" => Vector(ne = 1)
    case "nw" => Vector(nw = 1)
    case "sw" => Vector(ne = -1)
    case "se" => Vector(nw = -1)
    case "n"  => Vector(ne = 1, nw = 1)
    case "s"  => Vector(ne = -1, nw = -1)
  }

  case class Vector(ne: Int = 0, nw: Int = 0) {
    def +(v: Vector): Vector = Vector(ne + v.ne, nw + v.nw)

    val size: Int = abs(ne) + abs(nw) - min(abs(ne), abs(nw))
  }

}
