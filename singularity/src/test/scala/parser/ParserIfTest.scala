package parser

import util.ParseSpec

class ParserIfTest extends ParseSpec {

  it should "properly parse if" in {
    parser.parse("(if #t (+ 2 3) (- 5 10))") shouldBe IF(
      BOOL(true),
      LIST(List(ID("+"), INT(2), INT(3))),
      LIST(List(ID("-"), INT(5), INT(10)))
    )
  }
}
