package day10

import scala.Seq.range
import scala.collection.immutable.Stream.iterate

object KnotHash {

  def knotHash(input: String): String = {
    val salt = Seq(17, 31, 73, 47, 23)
    val sizes = input.map(_.toInt) ++ salt
    val sparseHash = iterate(Step(sizes = sizes))(hashRound)(64).hash
    val denseHash = sparseHash.sliding(16, 16).map(_.reduce(_ ^ _))
    denseHash.map(hex).mkString
  }

  def product(size: Int, values: Seq[Int]): Int =
    hash(size, values).take(2).product

  def hash(size: Int, sizes: Seq[Int]): Seq[Int] =
    hashRound(Step(hash = range(0, size), sizes = sizes)).hash

  case class Step(hash: Seq[Int] = range(0, 256),
                  sizes: Seq[Int],
                  position: Int = 0,
                  shift: Int = 0)

  private def hashRound(input: Step): Step =
    input.sizes.foldLeft(input)(
      (step, size) =>
        step.copy(hash = circularReverse(step.hash, step.position, size),
                  position = step.position + step.shift + size,
                  shift = step.shift + 1))

  private def hex(int: Int) = leftPad(BigInt(int).toString(16), 2, '0')

  private def leftPad(s: String, size: Int, char: Char): String =
    s.reverse.padTo(size, char).reverse

  private def circularReverse[T](seq: Seq[T], from: Int, size: Int): Seq[T] = {
    val start = from % seq.size
    shift(reverse(shift(seq, start), size), seq.size - start)
  }

  private def reverse[T](seq: Seq[T], size: Int): Seq[T] =
    seq.take(size).reverse ++ seq.drop(size)

  private def shift[T](seq: Seq[T], distance: Int): Seq[T] =
    seq.drop(distance) ++ seq.take(distance)

}
