package day09

object StreamProcessing {
  def garbageSize(stream: String): Int =
    modalFold(stream)(0) {
      case (Garbage, size, c) if !"!>".contains(c) => size + 1
      case (_, size, _)                            => size
    }

  def groupScore(stream: String): Int =
    modalFold(stream)((0, 1)) {
      case (Normal, (score, level), '{') => (score + level, level + 1)
      case (Normal, (score, level), '}') => (score, level - 1)
      case (_, (score, level), _)        => (score, level)
    } match { case (score, _) => score }

  private def modalFold[B](stream: String)(zero: B)(
      op: (Mode, B, Char) => B): B =
    stream.foldLeft((zero, Normal.asInstanceOf[Mode])) {
      case ((acc, Normal), '<')  => (op(Normal, acc, '<'), Garbage)
      case ((acc, Garbage), '>') => (op(Garbage, acc, '>'), Normal)
      case ((acc, Garbage), '!') => (op(Garbage, acc, '!'), Edit)
      case ((acc, Edit), ch)     => (op(Edit, acc, ch), Garbage)
      case ((acc, mode), ch)     => (op(mode, acc, ch), mode)
    } match { case (acc, _) => acc }

  trait Mode
  object Normal extends Mode
  object Garbage extends Mode
  object Edit extends Mode
}
