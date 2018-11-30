package day03

import java.lang.Math._

import scala.collection.immutable.Stream.from
import scala.collection.mutable

object SpiralMemory {

  def firstFibonachiAfter(value: Int): Int =
    from(1).map(fibonachi).dropWhile(_ <= value).head

  def fibonachi: Int => Int =
    fixMemo[Int, Int] { fibonachi => position =>
      if (position == 1) 1
      else neighbours(position).map(fibonachi).sum
    }(_)

  private def neighbours(position: Int): Seq[Int] =
    neighbours(toGrid(position)).map(fromGrid).filter(_ < position)

  private def neighbours(cell: (Int, Int)): Seq[(Int, Int)] = cell match {
    case (x, y) => for (dx <- -1 to 1; dy <- -1 to 1) yield (x + dx, y + dy)
  }

  def distance(position: Int): Int =
    toGrid(position) match {
      case (x, y) => abs(x) + abs(y)
    }

  def fromGrid(cell: (Int, Int)): Int = {
    val (x, y) = cell
    val index = max(abs(x), abs(y))
    val side = {
      if (index == x && index != -y) 0
      else if (index == y) 1
      else if (index == -x) 2
      else 3
    }
    val axisShift = List(y, -x, -y, x)(side)
    val sideSize = index * 2
    val minPosition = pow(max(sideSize - 1, 0), 2).toInt + 1
    val axisPos = max(index - 1, 0)
    minPosition + side * sideSize + axisPos + axisShift
  }

  def toGrid(position: Int): (Int, Int) = {
    val root = ceil(sqrt(position)).toInt
    val sideSize = if (root % 2 == 0) root else root - 1
    val index = sideSize / 2
    val axisPos = max(index - 1, 0)
    val minPosition = pow(max(sideSize - 1, 0), 2).toInt + 1
    val localIndex = position - minPosition
    val axisShift = if (index == 0) 0 else localIndex % sideSize - axisPos
    val side = if (index == 0) 0 else localIndex / sideSize
    List(index -> axisShift,
         -axisShift -> index,
         -index -> -axisShift,
         axisShift -> -index)(side)
  }

  //https://michid.wordpress.com/2009/02/23/function_mem/
  //https://en.wikipedia.org/wiki/Fixed-point_combinator
  private def fixMemo[I, O](fix: (I => O) => I => O): I => O = {
    lazy val io: I => O = memo()(fix(io)(_))
    io
  }

  private def memo[I, O](cache: mutable.Map[I, O] = mutable.HashMap[I, O]())(
      iToO: I => O): I => O =
    i => cache.getOrElseUpdate(i, iToO(i))
}
