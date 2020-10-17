package parser

import scala.util.matching.Regex
import scala.util.parsing.combinator._

object ExprParser {
  def apply(): ExprParser = new ExprParser()
}

class ExprParser extends RegexParsers {

  val num: Regex   = "[1-9][0-9]+".r
  val ident: Regex = "[a-zA-Z_]+".r

  def operator: Parser[String] = "+" | "-" | "*" | "/"

  def simpleExpr =
    "(" ~ operator ~ ident ~ ")"
}
