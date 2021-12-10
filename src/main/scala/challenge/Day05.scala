package challenge

import scala.io.Source
import lib.Points.{Point, Line}

object Day05:

  private val regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
  def parse(s: String): Line = s match
    case regex(x1, y1, x2, y2) => Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))

  def intersections(lines: List[Line]): Int =
    lines
      .flatMap(_.points())
      .groupBy(identity)
      .values
      .count(_.length > 1)

  val input: List[Line] = Source.fromResource("day05.txt").getLines().map(parse).toList
  def partOne(): Int    = intersections(input.filter(l => l.dx == 0 || l.dy == 0))
  def partTwo(): Int    = intersections(input)
