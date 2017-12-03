package day03

import java.lang.Math.{abs, ceil, max, sqrt}

import scala.collection.immutable.Stream.from
import scala.collection.mutable

object SpiralMemory {
  def firstFibonachiAfter(value: Int): Int =
    from(1).map(fibonachiAt).dropWhile(_ <= value).head

  def fibonachiAt: Int => Int =
    recMemo[Int, Int] { self => position =>
      if (position == 1) 1
      else neighbours(position).filter(_ < position).map(self).sum
    }(_)

  private def recMemo[I, O](recursive: ((I => O) => I => O)): I => O = {
    var cachedSelf: I => O = null
    cachedSelf = memo()(recursive(cachedSelf)(_))
    cachedSelf
  }

  private def memo[I, O](cache: mutable.Map[I, O] = mutable.HashMap[I, O]())(
      func: I => O): I => O =
    input => cache.getOrElseUpdate(input, func(input))

  private def neighbours(position: Int): Seq[Int] =
    gridNeighbours(toGrid(position)).map(fromGrid)

  private def gridNeighbours(cell: (Int, Int)): Seq[(Int, Int)] =
    for {
      x <- -1 to 1
      y <- -1 to 1
    } yield (cell._1 + x, cell._2 + y)

  def fromGrid(cell: (Int, Int)): Int = {
    val turn = SpiralTurn.fromGrid(cell)
    val side = {
      if (turn.index == cell._1 && turn.index != -cell._2) 0
      else if (turn.index == cell._2) 1
      else if (turn.index == -cell._1) 2
      else 3
    }
    val toAxis = side match {
      case 0 => cell._2
      case 1 => -cell._1
      case 2 => -cell._2
      case 3 => cell._1
    }
    turn.minPosition + (side * turn.sideSize + turn.sideAxis) + toAxis
  }

  def toGrid(position: Int): (Int, Int) = {
    val turn = turnOf(position)
    turn.toGrid(position - turn.minPosition)
  }

  def distance(position: Int): Int = {
    val cell = toGrid(position)
    abs(cell._1) + abs(cell._2)
  }

  def turnOf(position: Int): SpiralTurn = {
    val root = ceil(sqrt(position))
    val sqSide = if (root % 2 == 0) root + 1 else root
    val turn = ((sqSide - 1) / 2).intValue()
    SpiralTurn(turn)
  }

  case class SpiralTurn(index: Int) {
    val sideSize: Int = index * 2
    val sideAxis: Int = max(sideSize / 2 - 1, 0)
    val minPosition: Int = max(sideSize - 1, 0) * max(sideSize - 1, 0) + 1

    def toGrid(localIndex: Int): (Int, Int) = {
      val side = if (index == 0) 0 else localIndex / sideSize
      val toAxis = if (index == 0) 0 else localIndex % sideSize - sideAxis
      side match {
        case 0 => (index, toAxis)
        case 1 => (-toAxis, index)
        case 2 => (-index, -toAxis)
        case 3 => (toAxis, -index)
      }
    }
  }

  object SpiralTurn {
    def fromGrid(cell: (Int, Int)): SpiralTurn = {
      SpiralTurn(max(abs(cell._1), abs(cell._2)))
    }
  }
}
