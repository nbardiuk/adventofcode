package day02

object CorruptionChecksum {

  def spreadsheetChecksum(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.map(rowChecksum).sum

  def rowChecksum(row: Seq[Int]): Int = row.max - row.min

  def spreadsheetEdv(spreadsheet: Seq[Seq[Int]]): Int =
    spreadsheet.map(rowEdv).sum

  def rowEdv(row: Seq[Int]): Int =
    (for (a <- row; b <- row; if a % b == 0 && a != b) yield a / b).head
}
