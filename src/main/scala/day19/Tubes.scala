package day19

import scala.annotation.tailrec

object Tubes {

  def pathLength(map: String): Int = foldPath(map)(0)((_, count) => count + 1)

  def pathText(map: String): String =
    foldPath(map)("")((ch, text) => if (ch.isLetter) text :+ ch else text)

  private def foldPath[A](map: String)(zero: A)(acc: (Char, A) => A): A = {
    val rows = map.lines.toList
    def valueAt(p: (Int, Int)) = rows.lift(p._1).flatMap(_.lift(p._2))
    def add(a: (Int, Int), b: (Int, Int)) = (a._1 + b._1, a._2 + b._2)
    def turns(d: (Int, Int)) = Seq((d._2, d._1), (-d._2, -d._1))

    @tailrec
    def loop(pos: (Int, Int), dir: (Int, Int), result: A): A =
      valueAt(pos) match {
        case None      => result
        case Some(' ') => result
        case Some('+') =>
          turns(dir).find(d => valueAt(add(pos, d)).exists(_ != ' ')) match {
            case Some(d) => loop(add(pos, d), d, acc('+', result))
            case _       => result
          }
        case Some(ch) => loop(add(pos, dir), dir, acc(ch, result))
      }

    val start = (0, rows.head.indexOf('|'))
    val down = (1, 0)
    loop(start, down, zero)
  }

}
