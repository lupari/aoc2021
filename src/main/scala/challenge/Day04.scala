package challenge

import scala.io.Source
import scala.annotation.tailrec

object Day04:

  case class Player(board: List[Set[Int]]):
    def mark(n: Int): Player = copy(board = board.map(_ - n))
    val score: Int           = board.flatten.toSet.sum
    val hasBingo: Boolean    = board.exists(_.isEmpty)

  def toPlayer(lines: List[List[Int]]): Player = Player((lines ++ lines.transpose).map(_.toSet))

  def findScore()(fn: (List[Player]) => (Option[Player], List[Player])): Option[Int] =
    @tailrec
    def helper(xs: List[Int], ps: List[Player]): Option[Int] = xs match
      case Nil => None
      case h :: t =>
        fn(ps.map(_.mark(h))) match
          case (None, ps2)  => helper(t, ps2)
          case (Some(p), _) => Option(p.score * h)

    helper(numbers, players)

  val input: List[String] = Source.fromResource("day04.txt").getLines().toList
  val numbers: List[Int]  = input.head.split(",").map(_.toInt).toList
  val players: List[Player] =
    input
      .drop(2)
      .grouped(6)
      .map(_.filter(_.nonEmpty))
      .map(ls => toPlayer(ls.map(_.trim.split(" +").toList.map(_.toInt))))
      .toList

  def partOne(): Int = findScore()(ps => (ps.find(_.hasBingo), ps)).get
  def partTwo(): Int =
    def findLoser = (ps: List[Player]) =>
      val losers = ps.filterNot(_.hasBingo)
      if losers.isEmpty && ps.length == 1 then (Some(ps.head), Nil) else (None, losers)

    findScore()(findLoser).get
