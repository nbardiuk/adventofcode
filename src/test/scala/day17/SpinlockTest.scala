package day17

import day17.Spinlock.{after, valueAt}
import org.scalatest.FlatSpec

class SpinlockTest extends FlatSpec {

  "Spinlock" should "start at zero" in {
    assert(Spinlock.run(3)(0) == Seq(0))
  }
  it should "increment value next step" in {
    assert(Spinlock.run(3)(1) == Seq(0, 1))
  }
  it should "cycle around boundaries" in {
    assert(Spinlock.run(3)(0) == Seq(0))
    assert(Spinlock.run(3)(1) == Seq(0, 1))
    assert(Spinlock.run(3)(2) == Seq(0, 2, 1))
    assert(Spinlock.run(3)(3) == Seq(0, 2, 3, 1))
    assert(Spinlock.run(3)(4) == Seq(0, 2, 4, 3, 1))
    assert(Spinlock.run(3)(5) == Seq(0, 5, 2, 4, 3, 1))
    assert(Spinlock.run(3)(6) == Seq(0, 5, 2, 4, 3, 6, 1))
    assert(Spinlock.run(3)(7) == Seq(0, 5, 7, 2, 4, 3, 6, 1))
    assert(Spinlock.run(3)(8) == Seq(0, 5, 7, 2, 4, 3, 8, 6, 1))
    assert(Spinlock.run(3)(9) == Seq(0, 9, 5, 7, 2, 4, 3, 8, 6, 1))
  }

  "Value after 2017" should "be value in spinlock right after 2017 value" in {
    assert(after(value = 2017, step = 3, 2017) == 638)
  }
  it should "solve my input" in {
    assert(after(value = 2017, step = 335, 2017) == 1282)
  }

  "Value after 5000000" should "solve my input" in {
    assert(valueAt(1, 335, 50000000) == 27650600)
  }

}
