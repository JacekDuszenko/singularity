package parser

import util.ParseSpec

class ParserListConsTest extends ParseSpec {
  it should "parse list cons correctly" in {
    parser.parse("(list 1 2 3 4 5)") shouldBe List(
      LISTCONS(List(INT(1), INT(2), INT(3), INT(4), INT(5)))
    )
  }
}
