package day22

import scala.Function.tupled
import scala.collection.immutable.Stream.iterate

object Virus {
  def infected(iterations: Int, grid: Grid, position: Position): Int = {
    val bursts = iterate((grid, position))(tupled(dumbBurst)).take(iterations)
    bursts.zip(bursts.tail).count {
      case (before, after) => before._1.size < after._1.size
    }
  }

  def dumbBurst(grid: Grid, virus: Position): (Grid, Position) =
    if (grid.contains(virus.pos)) {
      val direction = right(virus.direction)
      (grid - virus.pos,
       virus.copy(direction = direction, pos = forward(virus.pos, direction)))
    } else {
      val direction = left(virus.direction)
      (grid + virus.pos,
       virus.copy(direction = direction, pos = forward(virus.pos, direction)))
    }

  def left(direction: Direction): Direction = direction match {
    case Up    => Left
    case Down  => Right
    case Left  => Down
    case Right => Up
  }

  def right(direction: Direction): Direction = direction match {
    case Up    => Right
    case Down  => Left
    case Left  => Up
    case Right => Down
  }

  def forward(cell: Cell, direction: Direction): Cell = direction match {
    case Up    => (cell._1, cell._2 - 1)
    case Down  => (cell._1, cell._2 + 1)
    case Left  => (cell._1 - 1, cell._2)
    case Right => (cell._1 + 1, cell._2)
  }

  type Cell = (Int, Int)
  type Grid = Set[Cell]

  case class Position(pos: Cell = (0, 0), direction: Direction = Up)

  sealed trait Direction
  object Up extends Direction
  object Down extends Direction
  object Left extends Direction
  object Right extends Direction

}
