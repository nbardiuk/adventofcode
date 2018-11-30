package day13

import scala.Function.tupled
import scala.collection.immutable.Stream.from

object PacketScanner {

  def delay(layers: Seq[(Int, Int)]): Int =
    from(0).find(delay => layersCaught(layers, delay).isEmpty).head

  def severity(layers: Seq[(Int, Int)]): Int =
    layersCaught(layers).map(tupled(_ * _)).sum

  def layersCaught(layers: Seq[(Int, Int)], delay: Int = 0): Seq[(Int, Int)] =
    layers.filter(tupled((index, size) => caught(index + delay, size)))

  private def caught(time: Int, size: Int) =
    time % (2 * size - 2) == 0
}
