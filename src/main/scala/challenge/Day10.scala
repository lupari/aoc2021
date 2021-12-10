package challenge

import scala.io.Source
import scala.annotation.tailrec

object Day10 {

  case class Inspection(error: Option[Char], unclosed: List[Char])
  case class Symbol(closure: Char, error: Int, completion: Int)
  val symbols = Map(
    '(' -> Symbol(')', 3, 1),
    '[' -> Symbol(']', 57, 2),
    '{' -> Symbol('}', 1197, 3),
    '<' -> Symbol('>', 25137, 4)
  )

  def errorScore(c: Char)      = symbols.find(_._2.closure == c).get._2.error
  def completionScore(c: Char) = symbols.find(_._2.closure == c).get._2.completion

  def autocomplete(i: Inspection): Long =
    @tailrec
    def helper(xs: List[Char], acc: Long): Long = xs match
      case Nil => acc
      case h :: t =>
        val closure = symbols(h).closure
        helper(t, acc * 5 + completionScore(closure))

    helper(i.unclosed.reverse, 0)

  def inspect(s: String): Inspection =
    @tailrec
    def helper(xs: List[Char], opened: List[Char]): Inspection = xs match
      case Nil => Inspection(None, opened)
      case h :: t =>
        symbols.find(_._2.closure == h) match
          case None =>
            helper(t, opened :+ h)
          case Some(x, y) =>
            if opened.last == x then helper(t, opened.dropRight(1))
            else Inspection(Option(h), Nil)

    helper(s.toList.tail, List(s.head))

  val input: List[String] = Source.fromResource("day10.txt").getLines().toList
  val inspected           = input.map(inspect)

  def partOne(): Int = inspected.flatMap(_.error).map(errorScore(_)).sum
  def partTwo(): Long =
    val scores = inspected.filter(_.unclosed.nonEmpty).map(i => autocomplete(i))
    scores.sorted.drop(scores.length / 2).head

}
