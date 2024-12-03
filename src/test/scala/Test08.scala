import org.scalatest._
import challenge.Day08

class Test08 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    //Day08.partOne() should be(532)
    Day08.partTwo() should be(1011284)
  }
