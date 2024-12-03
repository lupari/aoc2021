import org.scalatest._
import challenge.Day03

class Test03 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day03.partOne() should be(1997414)
    Day03.partTwo() should be(1032597)
  }
