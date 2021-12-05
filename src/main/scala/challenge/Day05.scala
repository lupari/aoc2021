package challenge

import scala.io.Source
import lib.Points.Point

object Day05 {

  def vents(p1: Point, p2: Point): Seq[Point] =
    val (dx, dy) = ((p2.x - p1.x).sign, (p2.y - p1.y).sign)
    val max      = math.max((p2.x - p1.x).abs, (p2.y - p1.y).abs)
    (0 to max).map(i => Point(p1.x + dx * i, p1.y + dy * i))

  private val regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
  def parse(s: String): (Point, Point) = s match
    case regex(x1, y1, x2, y2) => (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))

  def intersections(lines: List[(Point, Point)]): Int =
    lines
      .flatMap(l => vents(l._1, l._2))
      .groupBy(identity)
      .values
      .count(_.length > 1)

  val input: List[(Point, Point)] = Source.fromResource("day05.txt").getLines().map(parse).toList
  def partOne(): Int =
    intersections(input.filter(lines => lines._1.x == lines._2.x || lines._1.y == lines._2.y))
  def partTwo(): Int = intersections(input)

}
