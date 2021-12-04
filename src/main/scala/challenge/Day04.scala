package challenge

import scala.io.Source
import scala.annotation.tailrec
import lib.GridExtensions._
import lib.Points.Point

object Day04 {

  case class Square(value: Int, marked: Boolean = false)
  case class Player(board: Grid[Square]):
    def mark(n: Int): Player = board.find(_._2.value == n) match
      case Some((p, s)) => Player(board.updated(p, s.copy(marked = true)))
      case None         => this
    def hasBingo(): Boolean =
      def checkRow(i: Int) = board.filter(_._1.y == i).forall(_._2.marked == true)
      def checkCol(i: Int) = board.filter(_._1.x == i).forall(_._2.marked == true)
      (0 to 4).map(checkRow).exists(_ == true) || (0 to 4).map(checkCol).exists(_ == true)
    def score(): Int = board.values.filter(_.marked == false).map(_.value).sum

  def toBingoSheet(lines: List[List[Int]]): Grid[Square] = {
    for i <- 0 to 4; j <- 0 to 4
    yield (
      Point(j, i),
      Square(lines(i)(j))
    )
  }.toMap

  val input: List[String] = Source.fromResource("day04.txt").getLines().toList
  val numbers: List[Int]  = input.head.split(",").map(_.toInt).toList
  val players: List[Player] =
    input
      .drop(2)
      .grouped(6)
      .map(_.filter(_.nonEmpty))
      .map(ls => toBingoSheet(ls.map(_.trim.split(" +").toList.map(_.toInt))))
      .toList
      .map(Player.apply)

  def findScore()(fn: (List[Player]) => (Option[Player], List[Player])): Option[Int] =
    @tailrec
    def helper(xs: List[Int], ps: List[Player]): Option[Int] = xs match
      case Nil => None
      case h :: t =>
        fn(ps.map(p => p.mark(h))) match
          case (None, ps2)  => helper(t, ps2)
          case (Some(w), _) => Option(w.score() * h)

    helper(numbers, players)

  def partOne(): Int = findScore()(ps => (ps.find(_.hasBingo()), ps)).get
  def partTwo(): Int =
    def findLoser = (ps: List[Player]) =>
      val losers = ps.filterNot(_.hasBingo())
      if (losers.isEmpty && ps.length == 1) (Option(ps.head), Nil) else (None, losers)

    findScore()(findLoser).get

}
