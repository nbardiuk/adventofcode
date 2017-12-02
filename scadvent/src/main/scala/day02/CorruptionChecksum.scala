package day02

object CorruptionChecksum {

  def spreadsheetChecksum(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.map(rowChecksum).sum

  def rowChecksum(row: Seq[Int]): Int = row.max - row.min

  def spreadsheetEdv(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.map(rowEdv).sum

  def rowEdv(row: Seq[Int]): Int =
    row.flatMap(v => row.filter(_ % v == 0).filterNot(_ == v).map(_ / v)).head
}
