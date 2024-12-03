package lib

object Math:

  def mean(seq: Seq[Int]): Double = seq.sum / seq.length.toDouble

  def median(seq: Seq[Int]): Int =
    val sorted = seq.sorted
    if seq.size % 2 == 1 then sorted(seq.size / 2)
    else
      val (up, down) = sorted.splitAt(seq.size / 2)
      (up.last + down.head) / 2

  def arithmeticSeries(a: Int, b: Int, step: Int = 1, normalized: Boolean = false): Int =
    val dist     = (a - b).abs
    val (a1, an) = if normalized then (0, dist) else (a, b)
    val n        = dist / step + 1
    n * (a1 + an) / 2
