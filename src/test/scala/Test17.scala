import org.scalatest._
import challenge.Day17

class Test17 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day17.partOne() should be(10878)
    Day17.partTwo() should be(4716)
  }
