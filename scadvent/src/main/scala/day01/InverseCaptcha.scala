package day01

import scala.Function.tupled
import scala.annotation.tailrec

object InverseCaptcha {
  def sumMatchesHalfwayAround(number: String): Int =
    sumMatches(number.length / 2, number)

  def sumMatchesNext(number: String): Int = sumMatches(1, number)

  private def sumMatches(distance: Int, number: String) = {
    val digits = toDigits(number)
    val toMatch = shift(digits, distance)
    val matches = digits.zip(toMatch).filter(tupled(_ == _)).map(_._1)
    matches.sum
  }

  private def toDigits(number: String): Seq[Int] =
    number.toCharArray.map(code => code - '0')

  @tailrec
  private def shift[T](seq: Seq[T], times: Int): Seq[T] =
    if (times == 0 || seq.isEmpty) seq
    else shift(seq.tail :+ seq.head, times - 1)
}
