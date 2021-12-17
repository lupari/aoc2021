import org.scalatest._
import challenge.Day13

class Test13 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day13.partOne() should be(704)
  }

  it should "display HGAJBEHC" in {
    val canvas = Day13.partTwo()
    canvas foreach { row =>
      row.mkString foreach print; println()
    }
  }
