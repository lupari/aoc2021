import org.scalatest._
import challenge.Day14

class Test14 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day14.partOne() should be(2584)
    Day14.partTwo() should be(3816397135460L)
  }
