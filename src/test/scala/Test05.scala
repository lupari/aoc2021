import org.scalatest._
import challenge.Day05

class Test05 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day05.partOne() should be(5442)
    Day05.partTwo() should be(19571)
  }
