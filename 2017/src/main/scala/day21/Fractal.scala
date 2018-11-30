package day21

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseMatrix.{horzcat, vertcat}

import scala.collection.immutable.Stream.iterate
import scala.math.sqrt

object Fractal {

  type Image = DenseMatrix[Int]

  def countOn(image: Image, rules: Map[Image, Image], iterations: Int): Int =
    iterate(image)(expand(_, rules))
      .drop(iterations)
      .head
      .valuesIterator
      .count(_ == 1)

  def expand(image: Image, rules: Map[Image, Image]): Image = {
    val size = image.cols
    val side = if (size % 2 == 0) 2 else 3

    val corners = for (i <- 0 until size by side; j <- 0 until size by side) yield (i, j)
    val split = corners.map(corner => extract(image, corner, side))

    val enhanced = split.map(enhance(_, rules))

    joinRow(enhanced.grouped(size / side).toSeq.map(joinColon(_)))
  }

  def joinRow(images: Seq[Image]): Image = images.reduceLeft(horzcat(_, _))

  def joinColon(images: Seq[Image]): Image = images.reduceLeft(vertcat(_, _))

  def extract(image: Image, corner: (Int, Int), side: Int): Image =
    image(corner._2 until (corner._2 + side),
          corner._1 until (corner._1 + side))

  def enhance(image: Image, rules: Map[Image, Image]): Image =
    symetries(image).flatMap(rules.get).head

  def readRules(rules: Seq[String]): Map[Image, Image] =
    rules.map { rule =>
      val parts = rule.split(" => ", 2)
      (toImage(parts(0)), toImage(parts(1)))
    }.toMap

  def toImage(str: String): Image = {
    val values =
      str.filterNot("\n/".contains(_)).map(c => if (c == '#') 1 else 0).toArray
    val side = sqrt(values.length).toInt
    new DenseMatrix(side, side, values, 0, side, true)
  }

  def symetries(image: Image): Seq[Image] = {
    val rotations = iterate(image)(rotateRight).take(4)
    rotations ++ rotations.map(flip)
  }

  def flip(image: Image): Image =
    joinColon(
      (0 until image.rows).reverse.map(image(_, ::).inner.toDenseMatrix))

  def rotateRight(image: Image): Image = {
    val transposed = image.t
    joinRow((0 until image.rows).reverse.map(transposed(::, _).toDenseMatrix.t))
  }
}
