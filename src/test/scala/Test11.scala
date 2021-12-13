import org.scalatest._
import challenge.Day11

class Test11 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day11.partOne() should be(1655)
    Day11.partTwo() should be(337)
  }
