package lexer

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Devtalk extends AnyPropSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  // simple props
  property("startsWith") {
    forAll { (a: String, b: String) =>
      (a + b).startsWith(a)
    }
  }

  //really bad version of fib
  def fib(a: Int): Int = if (a <= 1) a else fib(a - 1) + fib(a - 2)

  //Warning! This will boil your cpu!
  property("fib") {
    forAll { (n: Int) =>
      fib(n) == fib(n - 1) + fib(n - 2)
    }
  }

  val smallInts = Gen.choose(3, 15)

  //This one should be easier on you
  property("smallFib") {
    forAll(smallInts) { (n: Int) =>
      println(s"executed on: $n")
      val fibComparisonResult = fib(n) == fib(n - 1) + fib(n - 2)
      fibComparisonResult shouldBe true
    }
  }

  //Different gens
  val variousGens = Seq(
    Gen.uuid,
    Gen.calendar,
    Gen.duration,
    Gen.alphaChar,
    Gen.alphaLowerChar,
    Gen.alphaStr,
    Gen.asciiStr
  )

  //Custom gen
  def isPrime(n: Int) = !Range.inclusive(2, Math.sqrt(n).toInt).exists(i => n % i == 0)

  //If the condition is too hard, ScalaCheck might not be able to generate enough values, and it might report a property test as undecided

  val badPrimeGen = for {
    n <- Gen.choose(14, 16)
    if isPrime(n)
  } yield n

  //Let's check some props of prime numbers
  property("prime bigger than two is odd (bad generator)") {
    forAll(badPrimeGen) { (prime: Int) =>
      println(prime)
      (prime % 2) shouldBe 1
    }
  }

  //Better approach
  val primes       = Range.inclusive(3, 500).filter(isPrime)
  val goodPrimeGen = Gen.oneOf(primes)

  //Let's check some props of prime numbers
  property("prime bigger than two is odd (good generator)") {
    forAll(goodPrimeGen) { (prime: Int) =>
      println(prime)
      (prime % 2) shouldBe 1
    }
  }

  //Creating case classes gen

  sealed trait Exporter

  final case class RaksExporter(amount:       Int)     extends Exporter
  final case class MkRpExporter(description:  String)  extends Exporter
  final case class WaproExporter(isImportant: Boolean) extends Exporter

  //This is clumsy, there is a better way
  val raksGen = for {
    a <- Arbitrary.arbitrary[Int]
  } yield RaksExporter(a)

  val mkRpGen = for {
    s <- Arbitrary.arbitrary[String]
  } yield MkRpExporter(s)

  val waproGen = for {
    b <- Arbitrary.arbitrary[Boolean]
  } yield WaproExporter(b)

  def exporterGen: Gen[Exporter] = Gen.oneOf(raksGen, mkRpGen, waproGen)

  property("just to show generated classes") {
    forAll(exporterGen) { (exporter: Exporter) =>
      println(exporter)
    }
  }

  // Arbitrary generator - generates arbitrary values for any supported type.
  // We can support custom types by
//
//  implicit lazy val arbitForCustomType: Arbitrary[CustomType] = Arbitrary(
//    oneOf(customTypeGeneratorOne, customTypeGeneratorTwo)
//  )
//  Arbitrary.arbitrary[RaksExporter]
  // After that we can skip passing the generator to property test executor

  //Test Case Minimisation

  val distinctListGen = List(
    List(1, 2, 3),
    List(2, 2),
    List(1, 2, 5, 10, 15, 22, 156, 1),
    List(1, 2, 3, 16, 42, 169, 1337, 1),
    List(1, 2, 2, 2, 2, 2, 2, 2),
    List(1, 2, 2, 2, 2, 2, 2, 2),
    List(1, 2, 2, 2, 2, 2, 2, 2),
    List(1, 2, 2, 2, 2, 2, 2, 2),
    List(1, 2, 2, 2, 2, 2, 2, 2),
    List(1, 2, 2, 2, 2, 2, 2, 2)
  )

  val minimizeFailCaseGen = Gen.oneOf(distinctListGen)

  property("list should be distinct") {
    forAll(minimizeFailCaseGen) { (distinctList: List[Int]) =>
      distinctList shouldBe distinctList.distinct
    }
  }
}
