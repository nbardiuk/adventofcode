package day18

import day18.Duet.firstSound
import org.scalatest.FlatSpec

class DuetTest extends FlatSpec {

  "First recovered sound" should "be value of the last played sound" in {
    assert(firstSound("""set a 2
        |snd a
        |rcv a
      """.stripMargin) == 2)
  }
  it should "recover only when register has non zero value" in {
    assert(firstSound("""set a -1
        |set b 2
        |add a 1
        |snd a
        |rcv a
        |jgz b -3
      """.stripMargin) == 1)
  }
  it should "match example" in {
    assert(firstSound("""set a 1
        |add a 2
        |mul a a
        |mod a 5
        |snd a
        |set a 0
        |rcv a
        |jgz a -1
        |set a 1
        |jgz a -2""".stripMargin) == 4)
  }
  it should "solve my input" in {
    assert(firstSound("""set i 31
        |set a 1
        |mul p 17
        |jgz p p
        |mul a 2
        |add i -1
        |jgz i -2
        |add a -1
        |set i 127
        |set p 680
        |mul p 8505
        |mod p a
        |mul p 129749
        |add p 12345
        |mod p a
        |set b p
        |mod b 10000
        |snd b
        |add i -1
        |jgz i -9
        |jgz a 3
        |rcv b
        |jgz b -1
        |set f 0
        |set i 126
        |rcv a
        |rcv b
        |set p a
        |mul p -1
        |add p b
        |jgz p 4
        |snd a
        |set a b
        |jgz 1 3
        |snd b
        |set f 1
        |add i -1
        |jgz i -11
        |snd a
        |jgz f -16
        |jgz a -19""".stripMargin) == 3188)
  }
}
