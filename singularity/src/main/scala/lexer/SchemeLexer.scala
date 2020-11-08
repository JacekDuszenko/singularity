package lexer

import lexer.ConstantTokenLexer.{bool, float, int, regex, str}
import lexer.SchemeLexer.{inParens, lp, rp}

import scala.util.parsing.combinator.Parsers

object SchemeLexer extends Parsers {
  val lp              = "\\(".r
  val rp              = "\\)".r

  def inParens[A](parser: Parser[A]): Parser[A] = {
    lp & (parser) & rp
  }
}

class SchemeLexer extends Lexer[LexToken[Any]] {

  val program = commandOrDefinition.*
  val commandOrDefinition = command | definition
  val command = expression
  val definition = inParens("define".r & variable & expression)

  val expression = variable | literal | procedureCall | lambdaExpression | conditional | assignment | derivedExpression
  val literal = selfEvaluating
  val selfEvaluating = boolean | number | character | string
  val procedureCall = inParens( operator & operand.* )
  val operator = expression
  val operand = expression
  val lambdaExpression = inParens("lambda".r & formals & body)
  val formals = inParens(variable.* ) | variable | inParens(variable.+ | "\\.".r | variable)
  val body = definition.* & sequence
  val sequence = command.* & sequence
  val conditional = inParens("if".r & test & consequent & alternate)
  val test = expression
  val consequent = expression
  val alternate = expression
  val assignment = inParens("set!".r & variable & expression )
  val derivedExpression = {
      inParens("and".r & test.*)
    | inParens("or".r & test.*)
    | inParens("let".r & inParens(bindingSpec.*) & body)
    | inParens("letrec".r & inParens(bindingSpec.*) & body)
  }

  val bindingSpec = inParens(variable & expression)

  val keyword = identifier




  val simpleDatum = boolean | number | character | string | symbol
  val symbol = identifier

  val compoundDatum = list
  val list = inParens(datum.*) | inParens((datum.+) ".".r & datum)
  val datum = simpleDatum | compoundDatum

  val singleQuote     = """"""".r
  val token           = identifier | boolean | number | character | string | lp | rp | ("#".r & lp) | "'".r | "`".r | ",".r | ",@".r | "\\.".r
  val whitespace      = " ".r | "\n".r
  val delimiter       = whitespace | "\\(".r | "\\)".r | ";".r | singleQuote
  val comment         = ";.*^".r
  val atmosphere      = whitespace | comment
  val intertokenSpace = atmosphere.*
  val identifier      = (initial & subsequent.*) | peculiarIdentifier
  val variable        = identifier
  val specialInitial = "!".r | "\\$".r | "%".r | "&".r | Predef
    .augmentString("*")
    .r | "/".r | ":".r | "<".r | "=".r | ">".r |
    "\\?".r | "\\^".r | "_".r | "~".r

  val letter  = "[a-z]".r
  val initial = specialInitial | letter
  val digit   = "0".r | "1".r | "2".r | "3".r | "4".r | "5".r | "6".r | "7".r | "8".r | "9".r

  val subsequent = initial | digit | specialSubsequent

  val specialSubsequent = "\\+".r | "-".r | "\\.".r | "@".r

  val peculiarIdentifier = "-".r | "\\.\\.\\.".r | "\\+".r

  val expressionKeyword = "quote".r | "lambda".r | "if".r | "set!".r | "begin".r | "cond".r | "and".r | "or".r | "case".r | "let".r |
    "letrec".r | "do".r | "delay".r | "quasiquote".r

  val syntacticKeyword = "else".r | "=>".r | "define".r | "unquote".r | "unquote-splicing".r | expressionKeyword

  val boolean = bool
  val character    = """#c""".r ~ ConstantTokenLexer.char
  val string  = str
  val number  = int | float

  override def apply() = ???

  override def lex(code: String) = ???

  override type Elem = this.type
}