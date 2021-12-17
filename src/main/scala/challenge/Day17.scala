package challenge

import scala.io.Source
import lib.Points.Point

object Day17:
  case class Probe(location: Point, velocity: Point):
    def move(): Probe =
      val vx = if velocity.x < 0 then 1 else if velocity.x > 0 then -1 else 0
      Probe(location + velocity, Point(velocity.x + vx, velocity.y - 1))
  case class Target(tl: Point, br: Point):
    def didHit(p: Point)   = p.x >= tl.x && p.x <= br.x && p.y <= tl.y && p.y >= br.y
    def didMiss(p: Point)  = p.x > br.x || p.y < br.y
    def awaiting(p: Point) = !didHit(p) && !didMiss(p)

  def shoot(p: Probe): Option[Probe] =
    val trajectory = Iterator
      .iterate(List(p))(ps => ps :+ ps.last.move())
      .dropWhile(ps => target.awaiting(ps.last.location))
      .next
    if target.didHit(trajectory.last.location) then Some(trajectory.maxBy(_.location.y)) else None

  private val regex = """.*x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)""".r
  def parseTarget(s: String): Target = s match
    case regex(x1, x2, y2, y1) => Target(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))

  val input  = Source.fromResource("day17.txt").mkString
  val target = parseTarget(input)
  val trajectories: Seq[Probe] =
    for x <- 1 to target.br.x; y <- target.br.y to -target.br.y yield Probe(Point.zero, Point(x, y))

  def partOne(): Int = trajectories.flatMap(shoot).maxBy(_.location.y).location.y
  def partTwo(): Int = trajectories.flatMap(shoot).size
