package lexer

import scala.util.parsing.combinator.Parsers

trait Lexer[A] extends Parsers {
  def lex(code:   String): ParseResult[A]
  def apply(code: String): ParseResult[A] = lex(code)
}
