package challenge

import scala.io.Source
import lib.GridExtensions._
import lib.Points.Point

object Day25:

  def moved(g: Grid[Char], c: Char)(next: (Point => Point)): Grid[Char] =
    val (movable, unmovable) = g.filter(_._2 == c).partition(p => g(next(p._1)) == '.')
    g.filterNot(_._2 == c) ++ unmovable ++ movable.map(kv => next(kv._1) -> kv._2)

  def step(g: Grid[Char]): Grid[Char] =
    val movedRight = moved(g, '>')(p => Point((p.x + 1) % maxX, p.y))
    moved(movedRight, 'v')(p => Point(p.x, (p.y + 1) % maxY))

  val grid         = Source.fromResource("day25.txt").mkString.toList.toGrid.withDefaultValue('.')
  val (maxX, maxY) = (grid.maxBy(_._1.x)._1.x + 1, grid.maxBy(_._1.y)._1.y + 1)

  def partOne(): Int = Iterator
    .iterate((grid, 1))((g, i) => (step(g), i + 1))
    .sliding(2)
    .dropWhile(x => x.head._1 != x.last._1)
    .next
    .head
    ._2
