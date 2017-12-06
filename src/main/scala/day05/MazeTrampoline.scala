package day05

import scala.Function.tupled
import scala.collection.immutable.Stream.iterate

object MazeTrampoline {
  def countStrangeJumps(commands: Array[Int]): Int = {
    iterate((commands, 0))(tupled(strangeJump)).takeWhile {
      case (coms, cursor) => coms.indices contains cursor
    }.size
  }

  def strangeJump(commands: Array[Int], cursor: Int): (Array[Int], Int) = {
    val command = commands(cursor)
    commands(cursor) = command + (if (command >= 3) -1 else 1)
    (commands, command + cursor)
  }

  def countJumps(commands: Array[Int]): Int =
    iterate((commands, 0))(tupled(jump)).takeWhile {
      case (coms, cursor) => coms.indices contains cursor
    }.size

  def jump(commands: Array[Int], cursor: Int): (Array[Int], Int) = {
    val command = commands(cursor)
    commands(cursor) = command + 1
    (commands, command + cursor)
  }
}
