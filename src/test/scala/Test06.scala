import org.scalatest._
import challenge.Day06

class Test06 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day06.partOne() should be(387413)
    Day06.partTwo() should be(1738377086345L)
  }
