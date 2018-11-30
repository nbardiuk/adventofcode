package day15

import java.lang.Integer.parseInt

import scala.Function.tupled
import scala.annotation.tailrec

object DuelingGenerators {

  def score1(left: Int, right: Int): Int = {
    val gen = zip(leftGen, rightGen)
    iterateCount((left, right), gen)(40000000)(tupled(matches))
  }

  def score2(left: Int, right: Int): Int = {
    val gen = zip(filter(leftGen)(_ % 4 == 0), filter(rightGen)(_ % 8 == 0))
    iterateCount((left, right), gen)(5000000)(tupled(matches))
  }

  private val leftGen = generator(16807)
  private val rightGen = generator(48271)

  private def generator(factor: Int): Gen[Int] =
    seed => (seed.toLong * factor % 2147483647).toInt

  def matches(left: Int, right: Int): Boolean =
    (left & mask) == (right & mask)

  private val mask = parseInt("1" * 16, 2)

  type Gen[T] = T => T

  private def zip[A, B](ga: Gen[A], gb: Gen[B]): Gen[(A, B)] = {
    case (a, b) => (ga(a), gb(b))
  }

  private def filter[T](gen: Gen[T])(cond: T => Boolean): Gen[T] =
    seed => {
      @tailrec
      def loop(value: T): T = if (cond(value)) value else loop(gen(value))

      loop(gen(seed))
    }

  private def iterateCount[T](seed: T, gen: Gen[T])(limit: Int)(
      cond: T => Boolean): Int = {

    @tailrec def loop(value: T, limit: Int, count: Int): Int =
      if (limit == 0) count
      else loop(gen(value), limit - 1, count + (if (cond(value)) 1 else 0))

    loop(gen(seed), limit, 0)
  }

}
