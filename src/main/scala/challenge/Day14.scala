package challenge

import scala.io.Source

object Day14:

  type Count = Map[String, Long]

  private val regex = """(.+) -> (.+)""".r
  def parse(s: String): (String, Char) = s match
    case regex(a, b) => (a, b.head)

  def insert(rules: Map[String, Char])(pairs: Count): Count =
    pairs.toList
      .flatMap((pair, count) =>
        List(
          (pair.head.toString + rules(pair), count),
          (rules(pair).toString + pair.last.toString, count)
        )
      )
      .groupMapReduce(_._1)(_._2)(_ + _)

  def count(c: Char, pairs: Count): Seq[Long] =
    val counts =
      pairs.toList.map((pair, count) => (pair.last, count)).groupMapReduce(_._1)(_._2)(_ + _)
    (counts + (c -> counts.get(c).map(_ + 1).getOrElse(1L))).values.toSeq

  val input  = Source.fromResource("day14.txt").getLines().toList
  val rules  = input.tail.tail.map(parse).toMap
  val counts = input.head.sliding(2).toList.groupMapReduce(_.mkString)(_ => 1L)(_ + _)

  def resolve(n: Int) =
    val lastCounts = Iterator.iterate(counts)(insert(rules)).drop(n).next
    val c          = count(input.head.head, lastCounts)
    c.max - c.min

  def partOne(): Long = resolve(10)
  def partTwo(): Long = resolve(40)
