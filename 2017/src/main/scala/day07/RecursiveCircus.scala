package day07

import scala.annotation.tailrec

object RecursiveCircus {
  def balancedWeight(input: Seq[((String, Int), Seq[String])]): Int = {
    val rootProgram = root(input)
    val branches = input.map { case ((name, _), children) => (name, children) }.toMap
    val weight = input.map(_._1).toMap

    def flatten(name: String): Stream[String] =
      name #:: branches(name).toStream.flatMap(flatten)

    def towerWeight(name: String): Int = flatten(name).map(weight).sum

    def mode[T](ts: Seq[T]): Option[T] =
      if (ts.isEmpty) None
      else Some(ts.groupBy(t => t).mapValues(_.size).maxBy(_._2)._1)

    @tailrec
    def go(base: String, diff: Int): Int = {
      val towers = branches(base).map(name => (name, towerWeight(name)))
      val balancedWeight = mode(towers.map(_._2))
      val unbalancedTower = balancedWeight.flatMap(balance => towers.find(_._2 != balance))

      unbalancedTower match {
        case None => weight(base) + diff
        case Some((program, towerWeight)) =>
          go(program, balancedWeight.fold(0)(_ - towerWeight))
      }
    }

    go(rootProgram, 0)
  }

  def root(branches: Seq[((String, Int), Seq[String])]): String = {
    val (roots, nonRoots) = branches.foldLeft((Set[String](), Set[String]())) {
      case ((roots, nonRoots), ((base, _), leaves)) =>
        (roots + base, nonRoots ++ leaves)
    }
    (roots diff nonRoots).head
  }

}
