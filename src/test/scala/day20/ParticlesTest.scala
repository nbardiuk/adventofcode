package day20

import day20.Particles.{closest, survived}
import org.scalatest.FlatSpec

import scala.io.Source.fromURL

class ParticlesTest extends FlatSpec {
  "The closest" should "be particle that is the closest to the (0,0,0) in long run" in {
    assert(closest(
      """p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
        |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
      """.stripMargin) == 0)
  }
  it should "solve my input" in {
    assert(closest(myInput) == 161)
  }

  "Survived" should "be number of particles that never collide" in {
    assert(survived(
      """p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
        |p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
        |p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
        |p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>""".stripMargin) == 1)
  }
  it should "solve my input" in {
    assert(survived(myInput) == 438)
  }

  private val myInput =
    fromURL(getClass.getResource("/day20/input.txt")).getLines().mkString("\n")
}
