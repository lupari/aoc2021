import org.scalatest._
import challenge.Day18

class Test18 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day18.partOne() should be(3524)
    Day18.partTwo() should be(4656)
  }
