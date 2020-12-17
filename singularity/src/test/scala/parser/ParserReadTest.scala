package parser

import model.{ID, READDEF}
import util.ParseSpec

class ParserReadTest extends ParseSpec {

  it should "parse read properly" in {
    parser.parse("(define h read)") shouldBe List(READDEF(ID("h")))
  }
}
