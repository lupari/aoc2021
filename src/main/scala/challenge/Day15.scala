package challenge

import scala.io.Source
import lib.Points.Point
import lib.Graphs

object Day15:
  import lib.GridExtensions._

  def expand(g: Grid[Int]): Grid[Int] =
    val end    = g.keys.maxBy(p => p.x * p.y)
    val (w, h) = (end.x + 1, end.y + 1)
    List
      .tabulate(5, 5)((x, y) =>
        g.map(kv => Point(x * w + kv._1.x, y * h + kv._1.y) -> (1 + (kv._2 - 1 + x + y) % 9))
      )
      .flatten
      .flatten
      .toMap

  def solve(g: Grid[Int]) =
    val goal: Point            = Point(g.maxBy(_._1.x)._1.x, g.maxBy(_._1.y)._1.y)
    def nf(p: Point)           = p.neighbors.filter(g.contains)
    def cf(a: Point, b: Point) = g(b)
    Graphs.dijkstra(Point.zero, goal)(nf)(cf)._2.get._2

  val grid = Source.fromResource("day15.txt").mkString.toList.toIntGrid

  def partOne(): Int = solve(grid)
  def partTwo(): Int = solve(expand(grid))
