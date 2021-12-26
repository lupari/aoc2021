package challenge

import scala.io.Source

object Day24:

  def findModelNumberStartingFrom(num: String, input: List[String]): Long =
    val blocks                  = input.grouped(18).toSeq
    def operand(i: Int)(n: Int) = blocks(i).map(_.split(' ').last)(n).toInt

    def step(acc: List[Int], stack: List[(Int, Int)], i: Int): (List[Int], List[(Int, Int)]) =
      // indexes 4, 5 and 15 are where things differentiate for each block
      val Seq(div, add1, add2) = Seq(4, 5, 15).map(operand(i))
      if div == 1 then (acc, stack :+ (i, add2))
      else
        val (j, added) = stack.last
        val acc2       = acc.updated(i, acc(j) + add1 + added)
        val res = acc2(i) match
          case x if x > 9 => acc2.updated(j, acc2(j) - x + 9).updated(i, 9)
          case x if x < 1 => acc2.updated(j, acc2(j) - x + 1).updated(i, 1)
          case _          => acc2
        (res, stack.init)

    Iterator
      .iterate((num.toList.map(_.asDigit), List.empty[(Int, Int)], 0))((res, stack, i) =>
        val (r2, s2) = step(res, stack, i)
        (r2, s2, i + 1)
      )
      .drop(14)
      .next
      ._1
      .mkString
      .toLong

  val input = Source.fromResource("day24.txt").getLines().toList

  def partOne(): Long = findModelNumberStartingFrom("99999999999999", input)
  def partTwo(): Long = findModelNumberStartingFrom("11111111111111", input)
