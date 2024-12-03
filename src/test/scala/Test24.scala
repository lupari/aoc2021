import org.scalatest._
import challenge.Day24

class Test24 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day24.partOne() should be(99394899891971L)
    Day24.partTwo() should be(92171126131911L)
  }
