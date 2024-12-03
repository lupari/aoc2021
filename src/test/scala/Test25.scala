import org.scalatest._
import challenge.Day25

class Test25 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day25.partOne() should be(378)
  }
