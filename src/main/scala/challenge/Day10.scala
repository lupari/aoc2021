package challenge

import scala.io.Source
import scala.annotation.tailrec

object Day10 {

  case class Line(error: Option[Char], unclosed: List[Char])
  case class Chunk(closure: Char, errorScore: Int, completionScore: Int)
  val chunks = Map(
    '(' -> Chunk(')', 3, 1),
    '[' -> Chunk(']', 57, 2),
    '{' -> Chunk('}', 1197, 3),
    '<' -> Chunk('>', 25137, 4)
  )

  def errorScore(c: Char)      = chunks.find(_._2.closure == c).get._2.errorScore
  def completionScore(c: Char) = chunks.find(_._2.closure == c).get._2.completionScore
  def autocomplete(l: Line): Long =
    l.unclosed.reverse.foldLeft(0L)((a, b) => a * 5 + completionScore(chunks(b).closure))

  def inspect(s: String): Line =
    @tailrec
    def helper(xs: List[Char], unclosed: List[Char]): Line = xs match
      case Nil => Line(None, unclosed)
      case h :: t =>
        chunks.find(_._2.closure == h) match
          case None =>
            helper(t, unclosed :+ h)
          case Some(x, _) =>
            if unclosed.last == x then helper(t, unclosed.dropRight(1))
            else Line(Option(h), Nil)

    helper(s.toList.tail, List(s.head))

  val input: List[String] = Source.fromResource("day10.txt").getLines().toList
  val lines: List[Line]   = input.map(inspect)

  def partOne(): Int = lines.flatMap(_.error).map(errorScore).sum
  def partTwo(): Long =
    val scores = lines.filter(_.unclosed.nonEmpty).map(autocomplete)
    scores.sorted.drop(scores.length / 2).head

}
