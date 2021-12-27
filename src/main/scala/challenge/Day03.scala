package challenge

import scala.io.Source
import scala.annotation.tailrec
import lib.Numbers._

object Day03:

  def commonest(xs: List[Char]): Char = if xs.count(_ == '0') > xs.length / 2 then '0' else '1'
  def rarest(xs: List[Char]): Char    = if commonest(xs) == '0' then '1' else '0'
  def findMatch(xs: List[String])(fn: (List[Char]) => Char): String =
    @tailrec
    def helper(xs: List[String], i: Int): String = xs match
      case h :: Nil => h
      case _ =>
        val c = xs.transpose.map(fn)(i)
        helper(xs.filter(x => x(i) == c), i + 1)

    helper(xs, 0)

  val input: List[String] = Source.fromResource("day03.txt").getLines().toList

  def partOne(): Long =
    val gamma   = input.transpose.map(commonest).mkString
    val epsilon = input.transpose.map(rarest).mkString
    bin2dec(gamma) * bin2dec(epsilon)

  def partTwo(): Long =
    val o   = findMatch(input)(commonest)
    val co2 = findMatch(input)(rarest)
    bin2dec(o) * bin2dec(co2)
