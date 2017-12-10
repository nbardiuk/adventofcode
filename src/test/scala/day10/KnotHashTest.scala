package day10

import day10.KnotHash.{hash, knotHash, product}
import org.scalatest.FlatSpec

class KnotHashTest extends FlatSpec {

  "Hashing" should "reverse items starting at 0" in {
    assert(hash(5, Seq(3)) == Seq(2, 1, 0, 3, 4))
    assert(hash(5, Seq(2)) == Seq(1, 0, 2, 3, 4))
  }
  it should "reverse after first" in {
    assert(hash(6, Seq(3, 2)) == Seq(2, 1, 0, 4, 3, 5))
  }
  it should "increase step by 1" in {
    assert(hash(10, Seq(3, 2, 2)) == Seq(2, 1, 0, 4, 3, 5, 7, 6, 8, 9))
    assert(hash(11, Seq(2, 2, 2, 2)) == Seq(1, 0, 3, 2, 4, 6, 5, 7, 8, 10, 9))
  }
  it should "wrap around the hash" in {
    assert(hash(5, Seq(3, 4)) == Seq(4, 3, 0, 1, 2))
  }
  it should "satisfy example" in {
    assert(hash(5, Seq(3, 4, 1, 5)) == Seq(3, 4, 2, 1, 0))
  }

  "Product" should "multiply first 2 values of the hash" in {
    assert(product(5, Seq(3, 4, 1, 5)) == 12)
  }
  it should "solve my input" in {
    val myInput =
      Seq(189, 1, 111, 246, 254, 2, 0, 120, 215, 93, 255, 50, 84, 15, 94, 62)
    assert(product(256, myInput) == 38415)
  }

  "Knot hash" should "satisfy examples" in {
    assert(knotHash("") == "a2582a3a0e66e6e86e3812dcb672a272")
    assert(knotHash("AoC 2017") == "33efeb34ea91902bb2f59c9920caa6cd")
    assert(knotHash("1,2,3") == "3efbe78a8d82f29979031a4aa0b16a9d")
    assert(knotHash("1,2,4") == "63960835bcdc130f0b66d7ff4f6a5a8e")
  }
  it should "solve my input" in {
    assert(
      knotHash("189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62") == "9de8846431eef262be78f590e39a4848")
  }

}
