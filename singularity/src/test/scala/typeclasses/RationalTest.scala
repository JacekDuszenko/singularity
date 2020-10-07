package typeclasses

import org.scalatest.Reporter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class RationalTest extends AnyFlatSpec {

  it should "throw IllegalArgumentException when rational initiated with 0 as denominator" in {
    assertThrows[IllegalArgumentException] {
      Rational(2, 0)
    }
  }

  it should "add two rationals correctly" in {
    val expectedRat = Rational(5, 6)

    val fst = Rational(1, 2)
    val snd = Rational(1, 3)

    fst + snd shouldBe expectedRat
  }

  it should "reduce rational to the simples form" in {
    val expectedRat = Rational(1, 2)

    val expandedRat = Rational(100, 200)

    expandedRat shouldBe expectedRat
  }

  it should "use test reporter in a method without throwing an exception" in {
    def fun(rep: Reporter): Unit = {
      print(rep)
    }

    fun(MockTestReporter)

    // no exception thrown
  }
}
