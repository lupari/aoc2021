package challenge

import scala.io.Source

object Day02 {

  trait Cmd
  case class Forward(amount: Int) extends Cmd
  case class Down(amount: Int)    extends Cmd
  case class Up(amount: Int)      extends Cmd

  trait Pos {
    def fwd(n: Int): Pos
    def down(n: Int): Pos
    def up(n: Int): Pos
    def location: Int
  }
  case class Pos1(horizontal: Int, depth: Int) extends Pos {
    override def fwd(n: Int): Pos  = copy(horizontal = horizontal + n)
    override def down(n: Int): Pos = copy(depth = depth + n)
    override def up(n: Int): Pos   = copy(depth = depth - n)
    override def location          = horizontal * depth
  }
  case class Pos2(horizontal: Int, depth: Int, aim: Int) extends Pos {
    override def fwd(n: Int): Pos  = copy(horizontal = horizontal + n, depth = depth + aim * n)
    override def down(n: Int): Pos = copy(aim = aim + n)
    override def up(n: Int): Pos   = copy(aim = aim - n)
    override def location          = horizontal * depth
  }

  def parse(cmd: String): Cmd = cmd.split(" ").toList match
    case (h :: t) =>
      val n = t.head.toInt
      h match
        case "forward" => Forward(n)
        case "down"    => Down(n)
        case "up"      => Up(n)
    case _ => throw new NoSuchElementException()

  def advance(pos: Pos, cmd: Cmd): Pos = cmd match
    case Forward(n) => pos.fwd(n)
    case Up(n)      => pos.up(n)
    case Down(n)    => pos.down(n)

  val input: List[Cmd] = Source.fromResource("day02.txt").getLines().map(parse).toList

  def partOne(): Int = input.foldLeft(Pos1(0, 0))((a: Pos, b) => advance(a, b)).location;
  def partTwo(): Int = input.foldLeft(Pos2(0, 0, 0))((a: Pos, b) => advance(a, b)).location

}
