import org.scalatest._
import challenge.Day02

class Test02 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day02.partOne() should be(1962940)
    Day02.partTwo() should be(1813664422)
  }
