package parser

import util.ParseSpec

class ParserPrimitivesTest extends ParseSpec {
  "Bool true token" should "be defined properly " in {
    parser.parse("#t") shouldBe List(BOOL(true))
  }

  "Bool false token" should "be defined properly " in {
    parser.parse("#f") shouldBe List(BOOL(false))
  }

  "List of int" should "be defined properly" in {
    parser.parse("(1 2 3)") shouldBe List(LIST(List(INT(1), INT(2), INT(3))))
  }

  "Char" should "be defined properly" in {
    parser.parse("'x'") shouldBe List(CHAR('x'))
  }
}
