package day13

import scala.collection.immutable.Stream.from
import scala.math.max

object PacketScanner {
  def delay(layers: Seq[(Int, Int)]): Int =
    from(0).find(delay => layersCaught(layers, delay).isEmpty).head

  def severity(layers: Seq[(Int, Int)]): Int =
    layersCaught(layers).map { case (layer, size) => layer * size }.sum

  def layersCaught(layers: Seq[(Int, Int)], delay: Int = 0): Seq[(Int, Int)] =
    layers.filter { case (layer, size) => scannerAtStart(size, layer + delay) }

  private def scannerAtStart(size: Int, time: Int): Boolean =
    time % (size + max(size - 2, 0)) == 0
}
