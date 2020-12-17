package parser

import model.{STRING, WRITE}
import util.ParseSpec

class ParserWriteTest extends ParseSpec {
  it should "parse write construction correctly" in {
    parser.parse("""(write "hehe")""") shouldBe List(WRITE(STRING("hehe")))
  }
}
