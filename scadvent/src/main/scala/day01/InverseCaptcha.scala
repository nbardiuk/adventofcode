package day01

import java.lang.Integer.parseInt

object InverseCaptcha {
  def sum(number: String): Int = {
    val digits = digs(number)
    val nextDigits = shift(digits)
    digits.zip(nextDigits)
      .filter { case (digit, next) => digit == next }
      .map { case (digit, _) => digit }.sum
  }

  private def digs(number: String): Seq[Int] = {
    number.toCharArray.map(c => parseInt("" + c))
  }

  private def shift[T](ns: Seq[T]): Seq[T] = {
    if (ns.isEmpty) ns else ns.tail :+ ns.head
  }
}
