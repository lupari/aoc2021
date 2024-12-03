package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day19:

  type Scanner   = Set[Point3]
  type BeaconMap = (Scanner, Map[Int, Point3])
  case class Point3(x: Int, y: Int, z: Int):
    def +(that: Point3)         = Point3(x + that.x, y + that.y, z + that.z)
    def -(that: Point3)         = Point3(x - that.x, y - that.y, z - that.z)
    def manhattan(that: Point3) = ((x - that.x).abs + (y - that.y).abs + (z - that.z).abs)
  object Point3:
    //https://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
    def orientations(p: Point3): Seq[Point3] =
      val Point3(x, y, z) = p
      Seq(
        Point3(x, y, z),
        Point3(x, -y, -z),
        Point3(-x, y, -z),
        Point3(-x, -y, z),
        Point3(x, z, -y),
        Point3(x, -z, y),
        Point3(-x, z, y),
        Point3(-x, -z, -y),
        Point3(y, z, x),
        Point3(y, -z, -x),
        Point3(-y, z, -x),
        Point3(-y, -z, x),
        Point3(y, x, -z),
        Point3(y, -x, z),
        Point3(-y, x, z),
        Point3(-y, -x, -z),
        Point3(z, x, y),
        Point3(z, -x, -y),
        Point3(-z, x, -y),
        Point3(-z, -x, y),
        Point3(z, y, -x),
        Point3(z, -y, x),
        Point3(-z, y, x),
        Point3(-z, -y, -x)
      )

  def orientations(scanner: Scanner): Seq[Scanner] =
    scanner.toSeq.map(Point3.orientations).transpose.map(_.toSet)

  def pair(s1: Scanner, s2: Scanner): Option[(Scanner, Point3)] =
    (for
      orientated <- orientations(s2)
      p1         <- s1
      p2         <- orientated
      dist = p1 - p2
      if (orientated.map(_ + dist) & s1).size > 11
    yield (orientated, dist)).headOption

  def findBeacons(scanners: Seq[Scanner]): BeaconMap =
    @tailrec
    def helper(
        xs: Seq[(Scanner, Int)],
        acc: Scanner,
        points: Map[Int, Point3]
    ): (Scanner, Map[Int, Point3]) = xs match
      case Nil => (acc, points)
      case _ =>
        val (scanner, i, scanner2, point) = xs
          .collect(x =>
            pair(acc, x._1) match
              case Some(pair) => Some(x._1, x._2, pair._1, pair._2)
              case _          => None
          )
          .flatten
          .head

        val beacons   = acc ++ scanner2.map(_ + point)
        val scanners2 = xs.filterNot(_ == (scanner, i))
        val points2   = points + (i -> point)
        helper(scanners2, beacons, points2)

    val indexed = scanners.zipWithIndex
    helper(indexed.tail, indexed.head._1, Map(0 -> Point3(0, 0, 0)))

  def parse(lines: List[String]): Scanner =
    def parse(line: String): Point3 =
      val parts = line.split(",").map(_.toInt).toList
      Point3(parts.head, parts.tail.head, parts.last)
    lines.map(parse).toSet

  def maxDistance(scanners: Map[Int, Point3]): Int =
    scanners.values.toList.combinations(2).map(xs => xs.head.manhattan(xs.last)).max

  val input: List[Scanner] =
    Source
      .fromResource("day19.txt")
      .mkString
      .split("\n\n")
      .toList
      .map(_.split("\n").toList.tail)
      .map(parse)
  lazy val result: BeaconMap = findBeacons(input)

  def partOne(): Int = result._1.size
  def partTwo(): Int = maxDistance(result._2)
