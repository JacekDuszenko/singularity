package lexer

import scala.util.parsing.combinator.RegexParsers

object SchemeLexer extends RegexParsers with Lexer[String] {
  type AnyValToken = String //replace with real tokens soon

  val program             = m(commandOrDefinition)(`*`)

  private def commandOrDefinition = command | definition
  private def command             = expression
  private def definition: Parser[AnyValToken]         = p("define".r ~ variable ~ expression ^^ {_.toString})

  private def expression       = variable | literal | procedureCall | lambdaExpression | conditional | assignment | derivedExpression
  private def literal          = selfEvaluating
  private def selfEvaluating   = boolean | number | character | string
  private def procedureCall    = p(operator ~ m(operand)(`*`) ^^ {_.toString})
  private def operator: Parser[AnyValToken]         = expression
  private def operand: Parser[AnyValToken]         = expression
  private def lambdaExpression = p("lambda".r ~ formals ~ body ^^{_.toString})
  private def formals          = p(m(variable)(`*`)) | variable | p(m(variable)(`+`) | "\\.".r | variable)
  private def body             = definition.* ~ sequence
  private def sequence  : Parser[AnyValToken]       = command.* ~ sequence ^^ {_.toString}
  private def conditional      = p("if".r ~ test ~ consequent ~ alternate ^^ {_.toString})
  private def test  : Parser[AnyValToken]           = expression
  private def consequent  : Parser[AnyValToken]     = expression
  private def alternate  : Parser[AnyValToken]      = expression
  private def assignment : Parser[AnyValToken]      = p("set!".r ~ variable ~ expression ^^ {_.toString})
  private def derivedExpression =
    (p("and".r ~ m(test)(`*`) ^^ {_.toString}) | p("or".r ~ m(test)(`*`) ^^ {_.toString})
    | p("let".r ~ p(m(bindingSpec)(`*`)) ~ body ^^ {_.toString})
    | p("letrec".r ~ p(m(bindingSpec)(`*`)) ~ body ^^ {_.toString}))


  private def bindingSpec: Parser[String] = p(variable ~ expression ^^ {_.toString})

  private def keyword     = identifier
  private def simpleDatum = boolean | number | character | string | symbol
  private def symbol      = identifier

  private def compoundDatum = list
  private def list          = p(m(datum)(`*`)) | (p(m(datum)(`+`) ~ "\\.".r ~ datum ^^ {_.toString}))
  private def datum  : Parser[AnyValToken]       = simpleDatum | compoundDatum

  private def singleQuote     = """"""".r
  private def token           = identifier | boolean | number | character | string | lp | rp | ("#".r ~ lp) | "'".r | "`".r | ",".r | ",@".r | "\\.".r
  private def whitespace      = " ".r | "\n".r
  private def delimiter       = whitespace | "\\(".r | "\\)".r | ";".r | singleQuote
  private def comment         = ";.*^".r
  private def atmosphere      = whitespace | comment
  private def intertokenSpace = atmosphere.*
  private def identifier      = initial ~ m(subsequent)(`*`) ^^ { _.toString }
  private def variable        = identifier
  private def specialInitial = "!".r | "\\$".r | "%".r | "~".r | "\\*".r | "/".r | ":".r | "<".r | "=".r | ">".r |
    "\\?".r | "\\^".r | "_".r | "&".r

  private def letter  = "[a-z]".r
  private def initial = specialInitial | letter
  private def digit   = "0".r | "1".r | "2".r | "3".r | "4".r | "5".r | "6".r | "7".r | "8".r | "9".r

  private def subsequent = initial | digit | specialSubsequent

  private def specialSubsequent = "\\+".r | "-".r | "\\.".r | "@".r
  private def expressionKeyword = "quote".r | "lambda".r | "if".r | "set!".r | "begin".r | "cond".r | "and".r | "or".r | "case".r | "let".r |
    "letrec".r | "do".r | "delay".r | "quasiquote".r

  private def syntacticKeyword = "else".r | "=>".r | "define".r | "unquote".r | "unquote-splicing".r | expressionKeyword

  private def boolean                            = "True".r | "False".r
  private def character                          = "\'.\'".r
  private def string                             = """"[^"]*"""".r
  private def number                             = "^-?\\d+$".r | simpleFloatReg | scientificNotationFloatReg
  private def simpleFloatReg             = "^[+-]?([0-9]*)?\\.[0-9]+$".r
  private def scientificNotationFloatReg = "-?[\\d.]+(?:E-?\\d+)?".r

  private def lp: Parser[AnyValToken] = "\\(".r ^^ identity
  private def rp: Parser[AnyValToken] = "\\)".r ^^ identity

  private def p(parser: Parser[AnyValToken]): Parser[AnyValToken] = {
    lp ~> (parser) <~ rp
  }

  private sealed trait MultiOpType
  private case object `*` extends MultiOpType
  private case object `+` extends MultiOpType

  private def m(parser: Parser[AnyValToken])(tp: MultiOpType): Parser[AnyValToken] = {
    val p = tp match {
      case `+` => parser.*
      case `*` => parser.+
    }
    p ^^ {_.mkString}
  }
  override def lex(c: String) = parse(program, c)
}
