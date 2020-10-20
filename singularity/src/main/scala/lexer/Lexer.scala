package lexer

import scala.util.parsing.combinator.Parsers

trait Lexer[A] extends Parsers {
  def combined: Parser[A]

  def lex(code: String): ParseResult[A]
}
