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

  val grid = Source.fromResource("day15.txt").mkString.toList.toIntGrid

  def partOne(): Int =
    val goal: Point            = Point(grid.maxBy(_._1.x)._1.x, grid.maxBy(_._1.y)._1.y)
    def nf(p: Point)           = p.neighbors.filter(grid.contains)
    def cf(a: Point, b: Point) = grid(b)
    Graphs.dijkstra(Point.zero, goal)(nf)(cf)._2.get._2

  def partTwo(): Int =
    val grid2                  = expand(grid)
    val goal: Point            = Point(grid2.maxBy(_._1.x)._1.x, grid2.maxBy(_._1.y)._1.y)
    def nf(p: Point)           = p.neighbors.filter(grid2.contains)
    def cf(a: Point, b: Point) = grid2(b)
    Graphs.dijkstra(Point.zero, goal)(nf)(cf)._2.get._2
