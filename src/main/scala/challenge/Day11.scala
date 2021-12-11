package challenge

import scala.io.Source
import scala.annotation.tailrec
import lib.Points.Point

object Day11 {
  import lib.GridExtensions._

  def evolve(xs: Grid[Int]): Grid[Int] =
    @tailrec
    def helper(prev: Set[Point], grid: Grid[Int]): Grid[Int] =
      val flashed = grid.filter(_._2 > 9).map(_._1).toSet -- prev
      if flashed.isEmpty then grid
      else
        val g2      = flashed.toList.flatMap(_.surroundings).groupMapReduce(identity)(_ => 1)(_ + _)
        val nextGen = grid.map(kv => (kv._1, kv._2 + g2.getOrElse(kv._1, 0))).toMap
        helper(flashed ++ prev, nextGen)

    helper(Set.empty, xs.map(kv => (kv._1, kv._2 + 1))).map(kv =>
      (kv._1, if kv._2 > 9 then 0 else kv._2)
    )

  val input: Grid[Int] = Source.fromResource("day11.txt").mkString.toList.toIntGrid

  def partOne(): Int =
    Iterator
      .iterate((input, 0))(x => (evolve(x._1), x._2 + x._1.count(_._2 == 0)))
      .drop(101)
      .next
      ._2

  def partTwo(): Int =
    Iterator
      .iterate(input)(evolve)
      .zipWithIndex
      .dropWhile(x => x._1.count(_._2 == 0) != 100)
      .next
      ._2

}
