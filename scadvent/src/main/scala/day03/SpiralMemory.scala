package day03

import java.lang.Math.{abs, floor, max}

import scala.collection.immutable.Stream.iterate

object SpiralMemory {
  def distance(value: Int): Int = spiralOf(value).index + distanceToCross(value)

  def distanceToCross(value: Int): Int = {
    if (value == 1) {
      0
    } else {
      val spiral = spiralOf(value)
      val sidePosition = ((value - spiral.previous.area) - 1) % spiral.side
      val cross = floor(spiral.radius / 2.0).intValue() - 1
      abs(cross - sidePosition)
    }
  }

  def spiralOf(value: Int): Spiral =
    iterate(Spiral(radius = 1, index = 0))(_.next).dropWhile(_.area < value).head

  case class Spiral(radius: Int, index: Int) {

    def previous: Spiral = Spiral(radius - 2, index - 1)

    def next: Spiral = Spiral(radius + 2, index + 1)

    def area: Int = radius * radius

    def side: Int = max(radius - 1, 1)
  }

}
