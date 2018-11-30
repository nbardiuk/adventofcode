package day02

import java.lang.Integer.parseInt

import day02.CorruptionChecksum.{
  rowChecksum,
  rowEdv,
  spreadsheetChecksum,
  spreadsheetEdv
}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class CorruptionChecksumTest extends FlatSpec {
  "Row checksum" should "be the difference between min and max value" in {
    assert(rowChecksum(Seq(5, 1, 9, 5)) == 8)
    assert(rowChecksum(Seq(7, 5, 3)) == 4)
    assert(rowChecksum(Seq(2, 4, 6, 8)) == 6)
  }

  "Spreadsheet checksum" should "be the sum of row checksums" in {
    assert(
      spreadsheetChecksum(
        Seq(
          Seq(5, 1, 9, 5), //
          Seq(7, 5, 3), //
          Seq(2, 4, 6, 8) //
        )) == 18)
  }

  it should "calculate my input" in {
    assert(spreadsheetChecksum(myInput) == 45351)
  }

  "Row edv" should "be the result of the only evenly division of values" in {
    assert(rowEdv(Seq(5, 9, 2, 8)) == 4)
    assert(rowEdv(Seq(9, 4, 7, 3)) == 3)
    assert(rowEdv(Seq(3, 8, 6, 5)) == 2)
  }

  "Spreadsheet edv" should "be sum of row edvs" in {
    assert(
      spreadsheetEdv(
        Seq(
          Seq(5, 9, 2, 8), //
          Seq(9, 4, 7, 3), //
          Seq(3, 8, 6, 5) //
        )) == 9)
  }
  it should "calculate my input" in {
    assert(spreadsheetEdv(myInput) == 275)
  }

  private def myInput =
    fromURL(getClass.getResource("/day02/input.csv"))
      .getLines()
      .map(_.split("\t"))
      .map(_.map(parseInt).toSeq)
      .toSeq
}
