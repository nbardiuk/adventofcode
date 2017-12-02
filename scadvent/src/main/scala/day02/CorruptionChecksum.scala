package day02

object CorruptionChecksum {

  def checksum(spreadsheet: Seq[Seq[Int]]): Int = spreadsheet.map(rowChecksum).sum

  def rowChecksum(row: Seq[Int]): Int = row.max - row.min

}
