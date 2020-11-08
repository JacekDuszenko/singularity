package lexer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.parsing.combinator.Parsers

class ConstantTokenLexerTest extends AnyFlatSpec with Parsers {

  val lexer: Lexer[_] = ConstantTokenLexer

  it should "recognize correct bool literal on string input from lexer" in {
    val falseStr: String = "#f"
    val trueStr: String  = "#t"

    val falseRes = lexer.lex(falseStr)
    falseRes.get shouldBe BOOL(false)

    val trueRes = lexer.lex(trueStr)
    trueRes.get shouldBe BOOL(true)
  }

  it should "recognize correct int literal on string input from lexer" in {
    lexer.lex("123").get shouldBe INT(123)
    lexer.lex("0").get shouldBe INT(0)
  }

  it should "recognize correct float literal on string input from lexer" in {
    lexer.lex("123.456").get shouldBe FLOAT(123.456f)
  }

  it should "recognize correct char literal on string input from lexer" in {
    lexer.lex("'a'").get shouldBe CHAR('a')
  }

  it should "recognize correct str literal on string input from lexer" in {
    val str =
      """
        |"hello world!"
        |""".stripMargin
    lexer.lex(str).get shouldBe STRING("hello world!")
  }
}
