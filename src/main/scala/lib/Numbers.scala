package lib

object Numbers {
  def leftPad(l: Int, s: String, c: Char = '0'): String = List.fill(l - s.length)(c).mkString + s
  def hex2bin(hex: Char): String =
    val bin = Integer.parseInt(hex.toString, 16).toBinaryString
    leftPad(4, bin)
  def hex2bin(hex: String): String = hex.flatMap(hex2bin(_))
  def bin2dec(bin: String): Long   = BigInt(bin, 2).longValue
}
