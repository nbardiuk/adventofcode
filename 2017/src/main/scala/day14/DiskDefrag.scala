package day14

import day10.KnotHash.knotHash

import scala.annotation.tailrec
import scala.collection.immutable.Stream.iterate
import scala.math.abs

object DiskDefrag {

  def row(keyword: String, row: Int): Seq[Int] =
    hexToBin(knotHash(s"$keyword-$row"))

  def used(keyword: String): Int = gridFor(keyword).map(_.count(_ == 1)).sum

  def regions(keyword: String): Int =
    iterate(gridFor(keyword))(removeRegion).takeWhile(notEmpty).size

  private def removeRegion(grid: Seq[Seq[Int]]): Seq[Seq[Int]] =
    regionAt(grid, positionOf(grid, 1)).foldLeft(grid)(freeAt)

  private def positionOf(grid: Seq[Seq[Int]], value: Int) = {
    val x = grid.indexWhere(_.contains(1))
    val y = grid(x).indexOf(value)
    (x, y)
  }

  private def freeAt(grid: Seq[Seq[Int]], pos: (Int, Int)) =
    grid.updated(pos._1, grid(pos._1).updated(pos._2, 0))

  private def regionAt(grid: Seq[Seq[Int]], pos: (Int, Int)) = {
    @tailrec
    def go(stack: List[(Int, Int)],
           region: Set[(Int, Int)]): Set[(Int, Int)] = {
      stack match {
        case Nil => region
        case cell +: rest =>
          val neighbours = neighboursOf(cell)
            .filter(inGrid)
            .filterNot(isEmpty(grid, _))
            .filterNot(region.contains)
          go(neighbours ++ rest, region + cell)
      }
    }

    go(List(pos), Set()).toSeq
  }

  private def neighboursOf(cell: (Int, Int)): List[(Int, Int)] =
    (for (i <- -1 to 1; j <- -1 to 1; if abs(i - j) == 1)
      yield (cell._1 + i, cell._2 + j)).toList

  private def isEmpty(grid: Seq[Seq[Int]], cell: (Int, Int)) =
    grid(cell._1)(cell._2) == 0

  private def inGrid(cell: (Int, Int)): Boolean =
    (0 to 127).contains(cell._1) && (0 to 127).contains(cell._2)

  private def notEmpty(grid: Seq[Seq[Int]]): Boolean =
    !grid.forall(_.forall(_ == 0))

  private def gridFor(keyword: String) = Seq.range(0, 128).map(row(keyword, _))

  private def hexToBin(hex: String): Seq[Int] = hex.flatMap {
    case '0' => Seq(0, 0, 0, 0)
    case '1' => Seq(0, 0, 0, 1)
    case '2' => Seq(0, 0, 1, 0)
    case '3' => Seq(0, 0, 1, 1)
    case '4' => Seq(0, 1, 0, 0)
    case '5' => Seq(0, 1, 0, 1)
    case '6' => Seq(0, 1, 1, 0)
    case '7' => Seq(0, 1, 1, 1)
    case '8' => Seq(1, 0, 0, 0)
    case '9' => Seq(1, 0, 0, 1)
    case 'a' => Seq(1, 0, 1, 0)
    case 'b' => Seq(1, 0, 1, 1)
    case 'c' => Seq(1, 1, 0, 0)
    case 'd' => Seq(1, 1, 0, 1)
    case 'e' => Seq(1, 1, 1, 0)
    case 'f' => Seq(1, 1, 1, 1)
  }
}
