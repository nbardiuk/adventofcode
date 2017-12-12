package day12

import scala.annotation.tailrec

object Plumber {
  def groups(links: String): Int = {
    val graph = parse(links)

    @tailrec
    def go(nodes: Set[String], result: Int): Int =
      if (nodes.isEmpty) result
      else go(nodes -- group(nodes.head, graph), result + 1)

    go(graph.keySet, 0)
  }

  def groupSize(links: String): Int = group("0", parse(links)).size

  private def group(node: String,
                    graph: Map[String, Set[String]]): Set[String] = {
    @tailrec
    def go(stack: List[String], result: Set[String]): Set[String] =
      stack match {
        case head +: tail =>
          go(tail ++ (graph(head) -- result), result ++ graph(head))
        case _ => result
      }

    go(List(node), Set())
  }

  private def parse(text: String): Map[String, Set[String]] =
    text.lines
      .map(line => {
        val link = line.split(" <-> ", 2)
        val node = link(0)
        val neighbours = link(1).split(", ").toSet
        (node, neighbours)
      })
      .toMap
}
