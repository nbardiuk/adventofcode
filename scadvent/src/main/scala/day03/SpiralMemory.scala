package day03

import java.lang.Math.{abs, max}

import scala.collection.immutable.Stream.iterate
import scala.collection.mutable

object SpiralMemory {
  def firstFibonachiAfter(value: Int): Int =
    iterate(1)(_ + 1).map(gridFibonachi).dropWhile(_ <= value).head

  def gridFibonachi: Int => Int = position => {
    def go(position: Int, cache: mutable.Map[Int, Int]): Int = {
      cache.getOrElseUpdate(position,
        neighbours(toGrid(position)).map(fromGrid).filter(_ < position).map(go(_, cache)).sum)
    }

    go(position, mutable.HashMap(1 -> 1))
  }

  private def neighbours(cell: (Int, Int)): Seq[(Int, Int)] = {
    for {
      x <- -1 to 1
      y <- -1 to 1
    } yield (cell._1 + x, cell._2 + y)
  }

  def fromGrid(grid: (Int, Int)): Int = {
    if (grid == (0, 0)) return 1
    val turn = max(abs(grid._1), abs(grid._2))
    val side = {
      if (turn == grid._1 && turn != -grid._2) 0
      else if (-grid._1 == turn) 2
      else if (grid._2 == turn) 1
      else 3
    }

    val index = SpiralIndex(turn)
    val axisShift = index.sideSize / 2 - 1
    val axis = side * index.sideSize + axisShift

    val toAxis = side match {
      case 0 => grid._2
      case 1 => -grid._1
      case 2 => -grid._2
      case 3 => grid._1
    }
    index.minPosition + axis + toAxis
  }

  def toGrid(position: Int): (Int, Int) = {
    if (position == 1) {
      (0, 0)
    } else {
      val index = indexOf(position)
      val side = index.localIndex / index.sideSize
      val axis = index.sideSize / 2 - 1
      val toAxis = index.localIndex % index.sideSize - axis
      side match {
        case 0 => (index.turn, toAxis)
        case 1 => (-toAxis, index.turn)
        case 2 => (-index.turn, -toAxis)
        case 3 => (toAxis, -index.turn)
      }
    }
  }

  def distance(position: Int): Int = {
    val grid = toGrid(position)
    abs(grid._1) + abs(grid._2)
  }

  def indexOf(position: Int): SpiralIndex =
    iterate(SpiralIndex(turn = 0))(_.nextTurn)
      .dropWhile(_.maxPosition < position)
      .map(si => si.copy(localIndex = position - si.minPosition))
      .head

  case class SpiralIndex(turn: Int, localIndex: Int = 0) {

    def nextTurn: SpiralIndex = SpiralIndex(turn + 1)

    def minPosition: Int = max(sideSize - 1, 0) * max(sideSize - 1, 0) + 1

    def maxPosition: Int = (sideSize + 1) * (sideSize + 1)

    def sideSize: Int = turn * 2
  }
}
