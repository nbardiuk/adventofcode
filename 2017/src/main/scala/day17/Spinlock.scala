package day17

import scala.annotation.tailrec
import scala.collection.immutable.Stream.iterate

object Spinlock {

  def valueAt(index: Int, step: Int, steps: Int): Int = {
    @tailrec
    def loop(size: Int, pos: Int, value: Int): Int = {
      if (size == steps) value
      else {
        val position = (pos + step) % size + 1
        loop(size = size + 1,
          pos = position,
          value = if (position == index) size else value)
      }
    }

    loop(size = 1, pos = 0, value = -1)
  }

  def after(value: Int, step: Int, steps: Int): Int = {
    val values = run(step)(steps)
    val index = (values.indexOf(value) + 1) % steps
    values(index)
  }

  def run(step: Int): Seq[Seq[Int]] =
    iterate(Buffer(0, Seq(0)))(nextStep(step, _)).map(_.items)

  case class Buffer(pos: Int, items: Seq[Int])

  private def nextStep(step: Int, buffer: Buffer): Buffer = {
    val size = buffer.items.size
    val nextPosition = (buffer.pos + step) % size + 1
    val nextItems = buffer.items.take(nextPosition) ++ (size +: buffer.items
      .drop(nextPosition))
    buffer.copy(pos = nextPosition, items = nextItems)
  }

}
