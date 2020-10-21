package lexer

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class LiteralTokenLexerProperty
    extends AnyPropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  private val lexer: Lexer[LiteralToken] = LiteralTokenLexer

  property("should literal int be mapped to int token") {
    forAll { num: Int =>
      lexer.lex(s"$num").get shouldBe INT(num)
    }
  }

  property("should char be mapped to char token") {
    forAll { c: Char => lexer.lex(s"'$c'").get shouldBe CHAR(c) }
  }

  property("should float be mapped to float token") {
    forAll { f: Float => lexer.lex(s"$f").get shouldBe FLOAT(f) }
  }

  property("should string be mapped to string token") {
    forAll { str: String =>
      val sfmt =
        s"""
           |"$str"
           |""".stripMargin
      lexer.lex(sfmt).get shouldBe STRING(str) }
  }
}
