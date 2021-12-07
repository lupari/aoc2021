package challenge

import scala.io.Source
import lib.Math.arithmeticSum

object Day07 {

  def cost()(fn: (Int, Int) => Int): Int =
    (crabs.min to crabs.max).map(i => crabs.map(fn(i, _)).sum).min

  val crabs: List[Int] = Source.fromResource("day07.txt").mkString.split(",").map(_.toInt).toList

  def partOne(): Int = cost()((a, b) => (a - b).abs)
  def partTwo(): Int = cost()(arithmeticSum)

}
