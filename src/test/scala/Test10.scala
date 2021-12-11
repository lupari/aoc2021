import org.scalatest._
import challenge.Day10

class Test10 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day10.partOne() should be(369105)
    Day10.partTwo() should be(3999363569L)
  }
