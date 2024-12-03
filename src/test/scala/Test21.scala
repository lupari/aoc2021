import org.scalatest._
import challenge.Day21

class Test21 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day21.partOne() should be(604998)
    Day21.partTwo() should be(157253621231420L)
  }
