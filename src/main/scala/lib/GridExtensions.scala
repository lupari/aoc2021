package lib

import lib.Points.Point

import scala.annotation.tailrec
import scala.reflect.ClassTag

object GridExtensions {

  type Grid[A] = Map[Point, A]

  extension [A](grid: Grid[A]) {
    def canvas(default: A)(cf: A => A)(using classTag: ClassTag[A]): Array[Array[A]] = {
      val (x, y) = (grid.keys.maxBy(_.x).x, grid.keys.maxBy(_.y).y)
      val canvas = Array.tabulate(y + 1, x + 1)((_, _) => default)
      for p <- grid yield canvas(p._1.y)(p._1.x) = cf(p._2)
      canvas
    }
  }

  extension (input: Seq[Char])
    def toGrid: Grid[Char] = {
      @tailrec
      def _toGrid(xs: Seq[Char], acc: Grid[Char], current: Point): Grid[Char] =
        xs match
          case h :: t if h == '\n' => _toGrid(t, acc, Point(0, current.y + 1))
          case h :: t => _toGrid(t, acc.updated(current, h), Point(current.x + 1, current.y))
          case _      => acc
      _toGrid(input, Map.empty, Point(0, 0))
    }

}
