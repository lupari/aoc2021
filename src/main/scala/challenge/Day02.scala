package challenge

import scala.io.Source
import lib.Points.{Point, Position}

object Day02:

  trait Cmd
  case class MoveHorizontal(amount: Int) extends Cmd
  case class MoveVertical(amount: Int)   extends Cmd

  trait Pos:
    val p: Point
    val product: Int = p.x * p.y
    def moveHorizontal(n: Int): Pos
    def moveVertical(n: Int): Pos

  case class Pos1(p: Point) extends Pos:
    override def moveHorizontal(n: Int) = Pos1(p + Point(n, 0))
    override def moveVertical(n: Int)   = Pos1(p + Point(0, n))

  case class Pos2(p: Point, aim: Int = 0) extends Pos:
    override def moveHorizontal(n: Int) = copy(p = p + Point(n, aim * n))
    override def moveVertical(n: Int)   = copy(aim = aim + n)

  private val regex = """(forward|down|up) (\d+)""".r
  def parse(cmd: String): Cmd = cmd match
    case regex(name, amount) =>
      name match
        case "forward" => MoveHorizontal(amount.toInt)
        case "down"    => MoveVertical(amount.toInt)
        case "up"      => MoveVertical(-amount.toInt)

  def advance(pos: Pos, cmd: Cmd): Pos = cmd match
    case MoveHorizontal(n) => pos.moveHorizontal(n)
    case MoveVertical(n)   => pos.moveVertical(n)

  val input: List[Cmd] = Source.fromResource("day02.txt").getLines().map(parse).toList

  def partOne(): Int = input.foldLeft(Pos1(Position.zero))(advance).product
  def partTwo(): Int = input.foldLeft(Pos2(Position.zero))(advance).product
