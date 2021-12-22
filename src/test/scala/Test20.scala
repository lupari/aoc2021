import org.scalatest._
import challenge.Day20

class Test20 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day20.partOne() should be(5354)
    Day20.partTwo() should be(18269)
  }
