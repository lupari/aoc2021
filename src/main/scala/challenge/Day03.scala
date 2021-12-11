package challenge

import scala.io.Source
import scala.annotation.tailrec

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

  def dec(s: String): Int = Integer.parseInt(s, 2)

  val input: List[String] = Source.fromResource("day03.txt").getLines().toList

  def partOne(): Int =
    val gamma   = input.transpose.map(commonest).mkString
    val epsilon = input.transpose.map(rarest).mkString
    dec(gamma) * dec(epsilon)

  def partTwo(): Int =
    val o   = findMatch(input)(commonest)
    val co2 = findMatch(input)(rarest)
    dec(o) * dec(co2)
