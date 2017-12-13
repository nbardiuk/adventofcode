package day13

import scala.collection.immutable.Stream.range

object PacketScanner {
  def severity(layers: Map[Int, Int]): Int =
    layersCaught(layers).map(layer => layers(layer) * layer).sum

  def layersCaught(layers: Map[Int, Int]): Seq[Int] =
    layers.filter { case (index, size) => scannerPositions(size)(index) == 0 }.keys.toSeq

  private def scannerPositions(size: Int): Stream[Int] =
    range(0, size) #::: range(size - 2, 0, -1) #::: scannerPositions(size)
}
