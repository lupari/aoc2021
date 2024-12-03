package challenge

import scala.io.Source
import scala.annotation.tailrec

object Day10:

  type Inspection = Either[Char, List[Char]]
  case class Chunk(closure: Char, errorScore: Int, completionScore: Int)
  val chunks = Map(
    '(' -> Chunk(')', 3, 1),
    '[' -> Chunk(']', 57, 2),
    '{' -> Chunk('}', 1197, 3),
    '<' -> Chunk('>', 25137, 4)
  )

  def errorScore(c: Char): Int = chunks.find(_._2.closure == c).get._2.errorScore
  def autocomplete(xs: List[Char]): Long =
    xs.reverse.foldLeft(0L)((a, b) => a * 5 + chunks(b).completionScore)

  def inspect(s: String): Inspection =
    @tailrec
    def helper(xs: List[Char], unclosed: List[Char]): Inspection = xs match
      case Nil => Right(unclosed)
      case h :: t =>
        chunks.find(_._2.closure == h) match
          case None =>
            helper(t, unclosed :+ h)
          case Some(x, _) =>
            if unclosed.last == x then helper(t, unclosed.init)
            else Left(h)

    helper(s.toList.tail, List(s.head))

  val input: List[String]     = Source.fromResource("day10.txt").getLines().toList
  val lines: List[Inspection] = input.map(inspect)

  def partOne(): Int = lines.collect({ case Left(e) => errorScore(e) }).sum
  def partTwo(): Long =
    val scores = lines.collect({ case Right(l) => autocomplete(l) })
    scores.sorted.drop(scores.length / 2).head
