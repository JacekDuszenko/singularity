package typeclasses

import org.scalatest.Reporter
import util.ParseSpec

class RationalTest extends ParseSpec {

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

  it should "get string from first trait method instead of second while mixing first trait as rightmost" in {
    class SomeImplementer extends SecondTrait with OneTrait

    val s   = new SomeImplementer
    val res = s.method()
    res shouldBe "firsty traity"
  }

  it should "collect cmd opts from string interpolation, then move them to seq, then concatenate with space" in {
    val cmdOpts =
      s"""native-image
           |-H:ConfigurationFileDirectories=/cfgs
           |--no-fallback
           |--allow-incomplete-classpath
           |--no-server
           |--static
           |--enable-all-security-services
           |--report-unsupported-elements-at-runtime
           |-jar nameOfJarFile.jar
           |""".stripMargin

    val asSeq = cmdOpts.split("\n")
    asSeq.length shouldBe 9
    val asCmdLine = asSeq.mkString(" ")
    print(asCmdLine)
  }
}
