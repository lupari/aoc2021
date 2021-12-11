package challenge

import scala.io.Source
import lib.Math._

object Day07:

  def fuelCost(n: Int)(costFn: (Int, Int) => Int): Int = crabs.map(costFn(n, _)).sum

  val crabs: List[Int] = Source.fromResource("day07.txt").mkString.split(",").map(_.toInt).toList

  def partOne(): Int = fuelCost(median(crabs))((a, b) => (a - b).abs)
  def partTwo(): Int =
    val avg = mean(crabs)
    Seq(avg.floor.toInt, avg.ceil.toInt)
      .map(fuelCost(_)((a, b) => arithmeticSeries(a, b, normalized = true)))
      .min
