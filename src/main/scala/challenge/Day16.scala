package challenge

import scala.io.Source
import lib.Numbers._

object Day16:

  trait Packet
  case class Literal(version: Long, value: Long)                          extends Packet
  case class Operator(version: Long, typeId: Long, packets: List[Packet]) extends Packet

  def parseLiteral(xs: String, version: Long) =
    val (blocks1, blocks0) = xs.drop(6).grouped(5).toList.span(_.head == '1')
    val value              = (blocks1 :+ blocks0.head).map(_.tail).mkString
    val size               = 6 + (blocks1.size + 1) * 5
    (size, Literal(version, bin2dec(value)))

  def parseOperator(xs: String, version: Long, typeId: Long, lengthId: Int): (Int, Packet) =
    val (size, packets) = lengthId match
      case 1 =>
        val packetCount = bin2dec(xs.drop(7).take(11)).toInt
        Iterator
          .iterate((7 + 11, List.empty[Packet]))((sz, p) => parse(xs, sz, p))
          .drop(packetCount)
          .next()
      case _ =>
        val packetsSize = bin2dec(xs.drop(7).take(15)) + 7 + 15
        Iterator
          .iterate((7 + 15, List.empty[Packet]))((sz, p) => parse(xs, sz, p))
          .dropWhile(_._1 < packetsSize)
          .next()
    (size, Operator(version, typeId, packets))

  def parse(xs: String, size: Int = 0, packets: List[Packet] = Nil): (Int, List[Packet]) =
    val data    = xs.drop(size)
    val version = bin2dec(data.take(3))
    val typeId  = bin2dec(data.drop(3).take(3))
    val (size2, packet2) =
      if typeId == 4 then parseLiteral(data, version)
      else parseOperator(data, version, typeId, data(6).asDigit)
    (size + size2, packets :+ packet2)

  def versionSum(packet: Packet): Long = packet match
    case Literal(version, _)           => version
    case Operator(version, _, packets) => version + packets.map(versionSum).sum

  def expression(packet: Packet): Option[Long] = packet match
    case Literal(_, value) => Some(value)
    case Operator(_, typeId, packets) =>
      (typeId, packets.flatMap(expression)) match
        case (0, xs)          => Some(xs.sum)
        case (1, xs)          => Some(xs.product)
        case (2, xs)          => Some(xs.min)
        case (3, xs)          => Some(xs.max)
        case (5, h :: i :: t) => Some(if h > i then 1 else 0)
        case (6, h :: i :: t) => Some(if h < i then 1 else 0)
        case (7, h :: i :: t) => Some(if h == i then 1 else 0)
        case _                => None

  val input: String  = hex2bin(Source.fromResource("day16.txt").mkString)
  val packet: Packet = parse(input)._2.head

  def partOne(): Long = versionSum(packet)
  def partTwo(): Long = expression(packet).get
