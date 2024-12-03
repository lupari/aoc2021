package challenge

import scala.io.Source

import lib.Points.Point
import lib.GridExtensions._
import lib.Numbers.bin2dec

object Day20:
  extension (c: Char) def toDigit: Int = if c == '#' then 1 else 0

  def neighbours(p: Point): Seq[Point] =
    for dy <- -1 to 1; dx <- -1 to 1 yield Point(p.x + dx, p.y + dy)

  case class Image(pixels: Map[Point, Int], min: Int, max: Int):
    def at(point: Point, default: Int): Int =
      bin2dec(neighbours(point).map(pixels.getOrElse(_, default)).mkString).toInt

  def parse(input: List[String]): (List[Int], Image) =
    val algorithm = input.head.map(toDigit).toList
    val data      = input.drop(2)
    val pixels =
      for y <- 0 until data.size; x <- 0 until data.size yield Point(x, y) -> data(y)(x).toDigit
    (algorithm, Image(pixels.toMap, -1, data.size))

  def step(algorithm: Seq[Int])(default: Int, img: Image): (Int, Image) =
    val pixels = for
      x <- img.min to img.max
      y <- img.min to img.max
    yield
      val i = img.at(Point(x, y), default)
      Point(x, y) -> algorithm(i)

    val nextDefault = if default == 0 then 1 else 0
    val nextGrid    = Image(pixels.toMap, img.min - 1, img.max + 1)
    (nextDefault, nextGrid)

  def enhance(n: Int): Int =
    Iterator.iterate((0, img))(step(algorithm)).drop(n).next()._2.pixels.values.sum

  val input            = Source.fromResource("day20.txt").getLines().toList
  val (algorithm, img) = parse(input)

  def partOne(): Int = enhance(n = 2)
  def partTwo(): Int = enhance(n = 50)
