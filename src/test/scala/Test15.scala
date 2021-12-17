import org.scalatest._
import challenge.Day15

class Test15 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day15.partOne() should be(790)
    Day15.partTwo() should be(2998)
  }
