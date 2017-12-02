package day02

import scala.collection.immutable.Stream.iterate

object CorruptionChecksum {

  def spreadsheetChecksum(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.map(rowChecksum).sum

  def rowChecksum(row: Seq[Int]): Int = row.max - row.min

  def spreadsheetEdv(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.map(rowEdv).sum

  def rowEdv(row: Seq[Int]): Int = {
    val unique = row.toSet //O(n)
    val max = row.max // O(n)
    def multiplesOf(value: Int) = iterate(2)(_ + 1).map(_ * value).takeWhile(_ <= max) // O(log(max))
    unique.flatMap(value => multiplesOf(value).find(unique.contains).map(_ / value)).head //O(n*log(max))
  }
}
