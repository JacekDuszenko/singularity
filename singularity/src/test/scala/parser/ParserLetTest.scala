package parser

import model.{BOOL, ID, INT, LAMBDA, LIST}
import util.ParseSpec

class ParserLetTest extends ParseSpec {

  "Let" should "be parsed correctly" in {
    parser.parse("(let ([a #t] [b 2] [c (lambda (x) (+ 2 x)) ]) (c b) )") shouldBe List(
      LIST(
        List(
          LAMBDA(List(ID("a"), ID("b"), ID("c")), LIST(List(ID("c"), ID("b")))),
          BOOL(true),
          INT(2),
          LAMBDA(List(ID("x")), LIST(List(ID("+"), INT(2), ID("x"))))
        )
      )
    )
  }
}
