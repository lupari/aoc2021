import org.scalatest._
import challenge.Day01

class Test01 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day01.partOne() should be(1184)
    Day01.partTwo() should be(1158)
  }
