package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ExprParserTest extends AnyFlatSpec {

  it should "parse basic scheme expr correctly" in {

    val res = parse("(+lol)")

    res shouldBe "(((~+)~lol)"
  }

  private def parse(code: String): String = {
    val p   = new ExprParser
    val res = p.parseAll(p.simpleExpr, code)
    res.get._1.toString
  }
}
