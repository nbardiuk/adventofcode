package day06

import scala.annotation.tailrec
import scala.collection.immutable.Stream.{from, iterate}

object MemoryReallocation {

  def loopSize(buckets: Seq[Int]): Int = {
    val reallocations = iterate(buckets)(reallocation)
    val cycleStart = indexOfDuplicate(reallocations)
    indexOfDuplicate(reallocations.drop(cycleStart))
  }

  def numberOfReallocations(buckets: Seq[Int]): Int =
    indexOfDuplicate(iterate(buckets)(reallocation))

  private def indexOfDuplicate(stream: Stream[Seq[Int]]): Int = {
    @tailrec
    def go[T](ts: Stream[T], seen: Set[T], result: Int): Int =
      if (seen contains ts.head) result
      else go(ts.tail, seen + ts.head, result + 1)

    go(stream, Set(), 0)
  }

  def reallocation(buckets: Seq[Int]): Seq[Int] = {
    val max = buckets.max

    val indexOfMax = buckets.indexOf(max)
    val withoutMax = buckets.updated(indexOfMax, 0)

    val div = max / buckets.size
    val mod = max % buckets.size
    val distance = buckets.size - indexOfMax - 1
    val distribution = shift(
      distance,
      from(0).take(buckets.size).map(i => div + (if (i < mod) 1 else 0)))

    (withoutMax, distribution).zipped.map(_ + _)
  }

  private def shift[T](distance: Int, seq: Seq[T]): Seq[T] =
    seq.drop(distance) ++ seq.take(distance)

}
