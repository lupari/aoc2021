package challenge

import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.Range.Inclusive

object Day22:

  extension (r: Inclusive)
    def volume(): Long = 1 + r.last - r.head
    def intersection(other: Inclusive): Option[Inclusive] =
      val (min, max)   = (r.head, r.last)
      val (min2, max2) = (other.head, other.last)
      if min2 <= min && max <= max2 then Some(r)
      else if min < min2 && max2 < max then Some(other)
      else if min2 <= min && min <= max2 then Some(min to max2)
      else if min <= min2 && min2 <= max then Some(min2 to max)
      else None

    def split(other: Inclusive): List[Inclusive] =
      val (min, max)   = (r.head, r.last)
      val (min2, max2) = (other.head, other.last)
      if min2 <= min && max <= max2 then List(r)
      else if min < min2 && max2 < max then List(min2 to max2, min to min2 - 1, max2 + 1 to max)
      else if min <= max2 && max2 < max then List(min to max2, max2 + 1 to max)
      else if min < min2 && min2 <= max then List(min2 to max2, min to min2 - 1)
      else Nil

  case class Cuboid(x: Inclusive, y: Inclusive, z: Inclusive):
    def volume: Long = x.volume() * y.volume() * z.volume()
    def intersection(other: Cuboid): Option[Cuboid] =
      for
        xs <- x.intersection(other.x)
        ys <- y.intersection(other.y)
        zs <- z.intersection(other.z)
      yield (Cuboid(xs, ys, zs))

    def split(other: Cuboid): List[Cuboid] =
      val xs = x.split(other.x)
      val ys = y.split(other.y)
      val zs = z.split(other.z)
      xs.tail.map(Cuboid(_, y, z)) ++ ys.tail.map(Cuboid(xs.head, _, z)) ++ zs.tail.map(Cuboid(xs.head, ys.head, _))

  trait Instruction:
    val cuboid: Cuboid
  case class TurnOn(cuboid: Cuboid)  extends Instruction
  case class TurnOff(cuboid: Cuboid) extends Instruction

  private val regex = """(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)""".r
  def parse(s: String): Instruction = s match
    case regex("on", x1, x2, y1, y2, z1, z2) =>
      TurnOn(Cuboid(x1.toInt to x2.toInt, y1.toInt to y2.toInt, z1.toInt to z2.toInt))
    case regex("off", x1, x2, y1, y2, z1, z2) =>
      TurnOff(Cuboid(x1.toInt to x2.toInt, y1.toInt to y2.toInt, z1.toInt to z2.toInt))

  def process(xs: List[Instruction]): Set[(Int, Int, Int)] =
    val bounds = Cuboid(-50 to 50, -50 to 50, -50 to 50)
    @tailrec
    def helper(xs: List[Instruction], acc: Set[(Int, Int, Int)]): Set[(Int, Int, Int)] = xs match
      case Nil => acc
      case h :: t =>
        h.cuboid.intersection(bounds) match
          case Some(c) =>
            val next = for x <- c.x; y <- c.y; z <- c.z yield (x, y, z)
            h match
              case TurnOn(_)  => helper(t, acc ++ next)
              case TurnOff(_) => helper(t, acc -- next)
          case None => helper(t, acc)

    helper(xs, Set.empty)

  def process2(xs: List[Instruction]): List[Cuboid] =
    @tailrec
    def helper(xs: List[Instruction], acc: List[Cuboid]): List[Cuboid] = xs match
      case Nil => acc
      case h :: t =>
        h match
          case TurnOn(c) =>
            acc.view.flatMap(_.intersection(c)).headOption match
              case None => helper(t, c :: acc)
              case Some(i) =>
                val remaining = h.cuboid.split(i).map(TurnOn(_))
                helper(remaining ++ t, acc)
          case TurnOff(c) =>
            val next = acc.flatMap(n =>
              n.intersection(h.cuboid) match
                case None    => List(n)
                case Some(i) => n.split(i)
            )
            helper(t, next)

    helper(xs, Nil)

  val input = Source.fromResource("day22.txt").getLines().map(parse).toList

  def partOne(): Int  = process(input).size
  def partTwo(): Long = process2(input).map(_.volume).sum
