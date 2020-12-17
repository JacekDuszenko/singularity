package parser

import model.{BOOL, ID, IF, INT, LIST}
import util.ParseSpec

class ParserIfTest extends ParseSpec {

  it should "properly parse if" in {
    parser.parse("(if #t (+ 2 3) (- 5 10))") shouldBe List(
      IF(
        BOOL(true),
        LIST(List(ID("+"), INT(2), INT(3))),
        LIST(List(ID("-"), INT(5), INT(10)))
      )
    )
  }
}
