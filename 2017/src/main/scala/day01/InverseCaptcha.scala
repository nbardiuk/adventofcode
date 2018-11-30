package day01

import scala.Function.tupled

object InverseCaptcha {
  def sumMatchesHalfwayAround(digits: String): Int =
    sumMatchesAt(digits.length / 2)(digits)

  def sumMatchesNext(digits: String): Int = sumMatchesAt(1)(digits)

  private def sumMatchesAt(distance: Int)(digits: String): Int =
    digits.zip(shift(digits, distance)).filter(tupled(_ == _)).map(_._1.asDigit).sum

  private def shift[T](seq: Seq[T], distance: Int): Seq[T] =
    seq.drop(distance) ++ seq.take(distance)
}
