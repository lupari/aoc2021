package challenge

import scala.io.Source

object Day08:

  type Digit = Set[Char]
  case class Entry(pattern: Seq[Digit], output: Seq[Digit])

  def find06(s1: Digit, s2: Digit, five: Digit) =
    if (five &~ s1).nonEmpty then (s1, s2) else (s2, s1)

  def find25(s1: Digit, s2: Digit, nine: Digit) =
    if (s1 &~ nine).nonEmpty then (s1, s2) else (s2, s1)

  def find3(s1: Digit, s2: Digit, s3: Digit) =
    s1 &~ (s2 | s3) match
      case s if s.isEmpty => (s1, (s2, s3))
      case _ =>
        if (s2 &~ (s1 | s3)).isEmpty then (s2, (s1, s3)) else (s3, (s1, s2))

  def find9(s1: Digit, s2: Digit, s3: Digit, three: Digit) =
    three &~ s1 match
      case s if s.isEmpty => (s1, (s2, s3))
      case _ =>
        if (three &~ s2).isEmpty then (s2, (s1, s3)) else (s3, (s1, s2))

  def decode(entry: Entry): Int =
    val Seq(f1, f2, f3)   = entry.pattern.filter(_.size == 5)
    val Seq(s1, s2, s3)   = entry.pattern.filter(_.size == 6)
    val (three, (f4, f5)) = find3(f1, f2, f3)
    val (nine, (s4, s5))  = find9(s1, s2, s3, three)
    val (two, five)       = find25(f4, f5, nine)
    val (zero, six)       = find06(s4, s5, five)
    val cipher            = Map(0 -> zero, 2 -> two, 3 -> three, 5 -> five, 6 -> six, 9 -> nine)

    def num(d: Digit): Option[Int] = predef.get(d.size).orElse(cipher.find(_._2 == d).map(_._1))

    entry.output.flatMap(num).mkString.toInt

  def parse(s: String): Entry =
    def _parse(xs: String) = xs.split(" ").map(_.toSet).toSeq
    val (pattern, output)  = (s.takeWhile(_ != '|'), s.dropWhile(_ != '|'))
    Entry(_parse(pattern), _parse(output))

  val entries: List[Entry]  = Source.fromResource("day08.txt").getLines().map(parse).toList
  val predef: Map[Int, Int] = Map(2 -> 1, 3 -> 7, 4 -> 4, 7 -> 8)

  def partOne(): Int = entries.flatMap(_.output).count(o => predef.contains(o.size))
  def partTwo(): Int = entries.map(decode).sum
