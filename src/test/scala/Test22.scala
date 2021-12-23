import org.scalatest._
import challenge.Day22

class Test22 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day22.partOne() should be(503864)
    Day22.partTwo() should be(1255547543528356L)
  }
