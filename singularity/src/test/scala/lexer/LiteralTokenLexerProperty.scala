package lexer

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class LiteralTokenLexerProperty
    extends AnyPropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  private val lexer: Lexer[LiteralToken] = LiteralTokenLexer

  property("prop") {
    forAll { num: Int =>
      lexer.lex(s"$num").get shouldBe INT(num)
    }
  }

}
