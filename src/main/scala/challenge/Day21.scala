package challenge

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

object Day21:
  case class Player(pos: Int, score: Int):
    def move(amount: Int): Player = {
      val pos2 = (pos + amount - 1) % 10 + 1
      Player(pos2, score + pos2)
    }

  def play(p1: Player, p2: Player): (Player, Player, Int) =

    @tailrec
    def helper(p1: Player, p2: Player, die: Int): (Player, Player, Int) =
      val movement1      = (1 to 3).map(i => (die + i) % 100).sum
      val movement2      = (4 to 6).map(i => (die + i) % 100).sum
      val (next1, next2) = (p1.move(movement1), p2.move(movement2))
      if next1.score > 999 then (next1, p2, die + 3)
      else if next2.score > 999 then (next2, next1, die + 6)
      else helper(next1, next2, die + 6)

    helper(p1, p2, 0)

  def play2(p1: Player, p2: Player): (Long, Long) =
    val memo = mutable.Map.empty[(Player, Player), (Long, Long)]
    val frequencies =
      (for
        r1 <- 1 to 3
        r2 <- 1 to 3
        r3 <- 1 to 3
      yield r1 + r2 + r3).groupMapReduce(identity)(_ => 1)(_ + _)

    def helper(p1: Player, p2: Player): (Long, Long) =
      memo.getOrElseUpdate(
        (p1, p2),
        if (p1.score > 20) then (1L, 0L)
        else if (p2.score > 20) then (0L, 1L)
        else
          val rolls =
            for
              (roll, rollCount) <- frequencies
              next1            = p1.move(roll)
              (p2Wins, p1Wins) = helper(p2, next1)
            yield (rollCount * p1Wins, rollCount * p2Wins)
          rolls.reduce({ case ((a, b), (c, d)) => (a + c, b + d) })
      )

    helper(p1, p2)

  val input        = Source.fromResource("day21.txt").getLines().toList
  val (pos1, pos2) = (input.head.last.asDigit, input.last.last.asDigit)

  def partOne(): Int =
    val (_, loser, rolls) = play(Player(pos1, 0), Player(pos2, 0))
    loser.score * rolls

  def partTwo(): Long =
    val (s1, s2) = play2(Player(pos1, 0), Player(pos2, 0))
    s1 max s2
