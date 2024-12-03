package challenge

import scala.io.Source
import lib.Points.Point

object Day13:
  import lib.GridExtensions._

  trait Fold
  case class XFold(i: Int) extends Fold
  case class YFold(i: Int) extends Fold

  private val pointRegex = """(\d+),(\d+)""".r
  def parsePoint(s: String): (Point, Char) = s match
    case pointRegex(a, b) => (Point(a.toInt, b.toInt), '#')

  private val foldRegex = """fold along (x|y)=(\d+)""".r
  def parseFold(s: String): Fold = s match
    case foldRegex("x", x) => XFold(x.toInt)
    case foldRegex("y", y) => YFold(y.toInt)

  def foldY(y: Int, grid: Grid[Char]): Grid[Char] =
    val (upper, lower) = grid.partition(_._1.y < y)
    val max            = grid.maxBy(_._1.y)._1.y
    val folded         = lower.map(kv => (Point(kv._1.x, max - kv._1.y), kv._2))
    folded ++ upper

  def foldX(x: Int, grid: Grid[Char]): Grid[Char] =
    val (left, right) = grid.partition(_._1.x < x)
    val max           = grid.maxBy(_._1.x)._1.x
    val folded        = right.map(kv => (Point(max - kv._1.x, kv._1.y), kv._2))
    folded ++ left

  def fold(f: Fold, g: Grid[Char]) = f match
    case XFold(x) => foldX(x, g)
    case YFold(y) => foldY(y, g)

  val input: List[String] = Source.fromResource("day13.txt").getLines().toList
  val grid: Grid[Char]    = input.takeWhile(_.nonEmpty).map(parsePoint).toMap
  val folds: List[Fold]   = input.dropWhile(!_.startsWith("fold")).map(parseFold)

  def partOne(): Int = fold(folds.head, grid).count(_._2 == '#')
  def partTwo(): Array[Array[Char]] =
    val g = folds.foldLeft(grid)((a, b) => fold(b, a))
    g.canvas(' ')(c => if c == '#' then '█' else c)
