package challenge

import scala.io.Source
import lib.Graphs

object Day12:

  type Caves = Map[String, Set[String]]
  case class Node(path: List[String], revisited: Boolean = false)

  def neighbors(node: Node): Set[Node] =
    caves(node.path.head)
      .filter(n => n.forall(_.isUpper) || !node.path.contains(n))
      .map(n => Node(n :: node.path))

  def neighbors2(node: Node): Set[Node] = node.path match
    case h :: t if h != "end" =>
      caves(h)
        .filter(n => n != "start" && (node.revisited || n.forall(_.isUpper) || !t.contains(n)))
        .map(n =>
          val revisit =
            if node.revisited && n.forall(_.isLower) && t.contains(n) then false
            else node.revisited
          Node(n :: node.path, revisit)
        )
    case _ => Set.empty

  def search(start: Node)(nf: (Node) => Set[Node]) =
    val nodes = Graphs.bfs(start)(nf)
    nodes.count(_._1.path.head == "end")

  def parse(input: List[String]): Caves =
    val parts = input.map(_.split('-').toList)
    val graph = parts.flatMap(x => Seq((x.head, x.last), (x.last, x.head)))
    graph.groupMapReduce(_._1)(kv => Set(kv._2))(_ ++ _)

  val caves: Caves = parse(Source.fromResource("day12.txt").getLines().toList)

  def partOne() = search(Node(List("start")))(neighbors)
  def partTwo() = search(Node(List("start"), revisited = true))(neighbors2)
