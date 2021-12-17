import org.scalatest._
import challenge.Day16

class Test16 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day16.partOne() should be(955)
    Day16.partTwo() should be(158135423448L)
  }
