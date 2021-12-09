package challenge

import scala.io.Source
import scala.annotation.tailrec

object Day09 {

  import lib.Points.Point
  import lib.GridExtensions._
  import lib.Graphs.BFS

  def isLowPoint(p: Point): Boolean = p.neighbors.forall(n => grid(n).asDigit > grid(p).asDigit)

  def basins(): List[Set[Point]] =
    @tailrec
    def helper(xs: Set[Point], acc: List[Set[Point]]): List[Set[Point]] = xs match
      case s if s.isEmpty => acc
      case s =>
        val basin = BFS(s.head)(_.neighbors.filter(grid(_).asDigit < 9)).keySet
        helper(s -- basin, acc :+ basin)

    helper(grid.keySet.filterNot(grid(_) == '9'), Nil)

  val grid: Grid[Char] =
    Source.fromResource("day09.txt").mkString.toList.toGrid.withDefaultValue('9')

  def partOne(): Int = grid.filter(kv => isLowPoint(kv._1)).map(_._2.asDigit + 1).sum
  def partTwo(): Int = basins().sortBy(_.size).takeRight(3).map(_.size).product

}
