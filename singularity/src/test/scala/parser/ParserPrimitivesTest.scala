package parser

import util.ParseSpec

class ParserPrimitivesTest extends ParseSpec {
  "Bool true token" should "be defined properly " in {
    parser.parse("#t") shouldBe BOOL(true)
  }

  "Bool false token" should "be defined properly " in {
    parser.parse("#f") shouldBe BOOL(false)
  }

  "List of int" should "be defined properly" in {
    parser.parse("(1 2 3)") shouldBe LIST(List(INT(1), INT(2), INT(3)))
  }

  "Char" should "be defined properly" in {
    parser.parse("'x'") shouldBe CHAR('x')
  }
}
