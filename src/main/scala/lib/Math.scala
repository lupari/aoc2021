package lib

object Math {

  def median(seq: Seq[Int]): Int =
    val sorted = seq.sorted
    if seq.size % 2 == 1 then sorted(sorted.size / 2)
    else
      val (up, down) = sorted.splitAt(seq.size / 2)
      (up.last + down.head) / 2

  def arithmeticSum(a: Int, b: Int): Int =
    val dist = (a - b).abs
    dist + (dist * (dist - 1) / 2)

}
