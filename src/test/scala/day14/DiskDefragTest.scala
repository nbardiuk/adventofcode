package day14

import day14.DiskDefrag.{regions, row, used}
import org.scalatest.FlatSpec

class DiskDefragTest extends FlatSpec {

  "Grid row" should "be 128 bit of keyword hash with row index" in {
    assert(row("flqrgnkx", 0).take(8) == Seq(1, 1, 0, 1, 0, 1, 0, 0))
    assert(row("flqrgnkx", 0).size == 128)
    assert(row("flqrgnkx", 1).take(8) == Seq(0, 1, 0, 1, 0, 1, 0, 1))
    assert(row("flqrgnkx", 1).size == 128)
  }

  "Used" should "be number of used squares in the grid" in {
    assert(used("flqrgnkx") == 8108)
  }
  it should "solve my input" in {
    assert(used("jzgqcdpd") == 8074)
  }

  "Regions" should "be the number of groups of used adjacent squares" in {
    assert(regions("flqrgnkx") == 1242)
  }
  it should "solve my input" in {
    assert(regions("jzgqcdpd") == 1212)
  }

}
