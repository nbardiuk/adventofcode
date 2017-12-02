package day02

import java.lang.Integer.parseInt

import day02.CorruptionChecksum.{checksum, rowChecksum}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class CorruptionChecksumTest extends FlatSpec {
  "Row checksum" should "be the difference between min and max value" in {
    assert(rowChecksum(Seq(5, 1, 9, 5)) == 8)
    assert(rowChecksum(Seq(7, 5, 3)) == 4)
    assert(rowChecksum(Seq(2, 4, 6, 8)) == 6)
  }

  "Spreadsheet checksum" should "be the sum of row checksums" in {
    assert(checksum(Seq(
      Seq(5, 1, 9, 5), //
      Seq(7, 5, 3), //
      Seq(2, 4, 6, 8) //
    )) == 18)
  }

  it should "calculate my input" in {
    val source = fromURL(getClass.getResource("/day02/input.csv"))
    val spreadsheet = source.getLines().map(_.split("\t")).map(_.map(parseInt).toSeq).toSeq
    assert(checksum(spreadsheet) == 45351)
  }
}
