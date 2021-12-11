package challenge

import scala.io.Source

object Day01:

  val input: List[Int] = Source.fromResource("day01.txt").getLines().map(_.toInt).toList

  def partOne(): Int = input.sliding(2).count(p => p.last > p.head)
  def partTwo(): Int = input.sliding(4).count(p => p.last > p.head)
