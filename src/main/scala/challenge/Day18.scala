package challenge

import scala.annotation.tailrec
import scala.io.Source

object Day18:
  case class Number(n: Int, level: Int)

  def parse(s: String): List[Number] =
    @tailrec
    def helper(xs: List[Char], level: Int, acc: List[Number]): List[Number] = xs match
      case Nil                 => acc
      case h :: t if h.isDigit => helper(t, level, acc :+ Number(h.asDigit, level))
      case '[' :: t            => helper(t, level + 1, acc)
      case ']' :: t            => helper(t, level - 1, acc)
      case _ :: t              => helper(t, level, acc)

    helper(s.toList, -1, Nil)

  def split(xs: List[Number], i: Int): List[Number] =
    val num      = xs(i)
    val (n1, n2) = ((num.n.toDouble / 2).floor.toInt, (num.n.toDouble / 2).ceil.toInt)
    xs.take(i) ++ List(Number(n1, num.level + 1), Number(n2, num.level + 1)) ++ xs.drop(i + 1)

  def explode(xs: List[Number], i: Int): List[Number] = i match
    case 0 =>
      val Seq(n2, n3) = xs.tail.take(2)
      List(Number(0, 3), Number(n2.n + n3.n, n3.level)) ++ xs.drop(3)
    case x if x < xs.length - 2 =>
      val Seq(n0, n1, n2, n3) = xs.slice(i - 1, i + 3)
      val mid = List(Number(n0.n + n1.n, n0.level), Number(0, 3), Number(n2.n + n3.n, n3.level))
      xs.take(x - 1) ++ mid ++ xs.drop(x + 3)
    case _ =>
      val Seq(n1, n2, n3) = xs.takeRight(3)
      xs.take(i - 1) ++ List(Number(n1.n + n2.n, 3), Number(0, 3))

  def reduce(xs: List[Number]): List[Number] =
    Iterator
      .iterate((xs, false))((xs, _) =>
        xs.zipWithIndex.find(_._1.level == 4).map(_._2) match
          case Some(i) => (explode(xs, i), false)
          case None =>
            xs.zipWithIndex.find(_._1.n > 9).map(_._2) match
              case Some(i) => (split(xs, i), false)
              case None    => (xs, true)
      )
      .dropWhile(_._2 == false)
      .next
      ._1

  def join(a: List[Number], b: List[Number]): List[Number] = reduce(
    (a ++ b).map(num => Number(num.n, num.level + 1))
  )

  @tailrec
  def magnitude(xs: List[Number]): Int = xs match
    case h :: Nil => h.n
    case _ =>
      val maxLevel     = xs.maxBy(_.level).level
      val (head, tail) = xs.span(_.level != maxLevel)
      val (n1, n2)     = (tail.head.n, tail.tail.head.n)
      magnitude((head :+ Number(3 * n1 + 2 * n2, maxLevel - 1)) ++ tail.drop(2))

  val input: List[List[Number]] = Source.fromResource("day18.txt").getLines().map(parse).toList

  def partOne(): Int = magnitude(input.reduce(join))
  def partTwo(): Int = input
    .combinations(2)
    .flatMap(_.permutations.map(p => magnitude(join(p.head, p.last))))
    .max
