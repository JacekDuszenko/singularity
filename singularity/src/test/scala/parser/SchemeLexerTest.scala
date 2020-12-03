package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SchemeLexerTest extends AnyFlatSpec with Matchers {
  val lexer = SchemeParser

  "Bool true token" should "be defined properly " in {
    SchemeParser.parse("#t") shouldBe BOOL(true)
  }

  "Bool false token" should "be defined properly " in {
    SchemeParser.parse("#f") shouldBe BOOL(false)
  }

  "List of int" should "be defined properly" in {
    SchemeParser.parse("(1 2 3)") shouldBe LIST(List(INT(1), INT(2), INT(3)))
  }
}
