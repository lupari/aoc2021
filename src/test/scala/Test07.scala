import org.scalatest._
import challenge.Day07

class Test07 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day07.partOne() should be(329389)
    Day07.partTwo() should be(86397080)
  }
