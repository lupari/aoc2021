import org.scalatest._
import challenge.Day09

class Test09 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day09.partOne() should be(564)
    Day09.partTwo() should be(1038240)
  }
