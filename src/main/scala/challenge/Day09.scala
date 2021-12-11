package challenge

import scala.io.Source
import scala.annotation.tailrec
import lib.Points.Point
import lib.Graphs

object Day09:

  import lib.GridExtensions._

  def isLowPoint(p: Point): Boolean = p.neighbors.forall(n => grid(n) > grid(p))

  lazy val basins: List[Set[Point]] =
    @tailrec
    def helper(xs: Set[Point], acc: List[Set[Point]]): List[Set[Point]] = xs match
      case s if s.isEmpty => acc
      case s =>
        val basin = Graphs.bfs(s.head)(_.neighbors.filterNot(grid(_) == 9)).keySet
        helper(s -- basin, acc :+ basin)

    helper(lowPoints.keySet, Nil)

  val grid: Grid[Int] =
    Source.fromResource("day09.txt").mkString.toList.toIntGrid.withDefaultValue(9)
  val lowPoints: Grid[Int] = grid.filter(kv => isLowPoint(kv._1))

  def partOne(): Int = lowPoints.map(_._2 + 1).sum
  def partTwo(): Int = basins.map(_.size).sorted.takeRight(3).product
