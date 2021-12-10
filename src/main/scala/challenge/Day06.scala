package challenge

import scala.io.Source

object Day06:

  def evolve(population: Map[Int, Long]): Map[Int, Long] =
    val population2 = population.filter(_._1 > 0).map(p => p._1 - 1 -> p._2)
    val eights      = population.get(0).getOrElse(0L)
    val sixes       = eights + population.get(7).getOrElse(0L)
    population2 + (6 -> sixes) + (8 -> eights)

  val input: List[Int] = Source.fromResource("day06.txt").mkString.split(",").map(_.toInt).toList
  val population: Map[Int, Long] = input.groupMapReduce(identity)(_ => 1L)(_ + _)

  def partOne(): Long = Iterator.iterate(population)(evolve).drop(80).next.values.sum
  def partTwo(): Long = Iterator.iterate(population)(evolve).drop(256).next.values.sum
