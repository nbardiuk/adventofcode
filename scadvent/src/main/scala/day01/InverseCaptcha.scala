package day01

object InverseCaptcha {
  def sumNext(number: String): Int = {
    val digits = toDigits(number)
    val nextDigits = shift(digits)
    digits
      .zip(nextDigits)
      .withFilter { case (digit, next) => digit == next }
      .map { case (digit, _) => digit }
      .sum
  }

  private def toDigits(number: String): Seq[Int] = {
    number.toCharArray.map(code => code - '0')
  }

  private def shift[T](seq: Seq[T]): Seq[T] = {
    if (seq.isEmpty) seq else seq.tail :+ seq.head
  }
}
