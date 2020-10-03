import org.scalatest.flatspec.AnyFlatSpec

class RationalTest extends AnyFlatSpec {

  it should "throw IllegalArgumentException when rational initiated with 0 as denominator" in {
    assertThrows[IllegalArgumentException] {
      Rational(2, 0)
    }
  }
}
