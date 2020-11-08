package lexer

import lexer.SchemeLexer.{AnyValToken, program}

import scala.util.parsing.combinator.RegexParsers

object SchemeLexer extends RegexParsers {
  type AnyValToken = String //replace with real tokens soon

  val program             = m(commandOrDefinition)(`*`)

  private val commandOrDefinition = command | definition
  private val command             = expression
  private val definition          = p("define".r ~ variable ~ expression ^^ {_.toString})

  private val expression       = variable | literal | procedureCall | lambdaExpression | conditional | assignment | derivedExpression
  private val literal          = selfEvaluating
  private val selfEvaluating   = boolean | number | character | string
  private val procedureCall    = p(operator ~ m(operand)(`*`) ^^ {_.toString})
  private val operator         = expression
  private val operand          = expression
  private val lambdaExpression = p("lambda".r ~ formals ~ body ^^{_.toString})
  private val formals          = p(m(variable)(`*`)) | variable | p(m(variable)(`+`) | "\\.".r | variable)
  private val body             = definition.* ~ sequence
  private val sequence         = command.* ~ sequence
  private val conditional      = p("if".r ~ test ~ consequent ~ alternate ^^ {_.toString})
  private val test             = expression
  private val consequent       = expression
  private val alternate        = expression
  private val assignment       = p("set!".r ~ variable ~ expression ^^ {_.toString})
  private val derivedExpression =
    (p("and".r ~ m(test)(`*`) ^^ {_.toString}) | p("or".r ~ m(test)(`*`) ^^ {_.toString})
    | p("let".r ~ p(m(bindingSpec)(`*`)) ~ body ^^ {_.toString})
    | p("letrec".r ~ p(m(bindingSpec)(`*`)) ~ body ^^ {_.toString}))


  private val bindingSpec: Parser[String] = p(variable ~ expression ^^ {_.toString})

  private val keyword     = identifier
  private val simpleDatum = boolean | number | character | string | symbol
  private val symbol      = identifier

  private val compoundDatum = list
  private val list          = p(m(datum)(`*`)) | (p(m(datum)(`+`) ~ "\\.".r ~ datum ^^ {_.toString}))
  private val datum         = simpleDatum | compoundDatum

  private val singleQuote     = """"""".r
  private val token           = identifier | boolean | number | character | string | lp | rp | ("#".r ~ lp) | "'".r | "`".r | ",".r | ",@".r | "\\.".r
  private val whitespace      = " ".r | "\n".r
  private val delimiter       = whitespace | "\\(".r | "\\)".r | ";".r | singleQuote
  private val comment         = ";.*^".r
  private val atmosphere      = whitespace | comment
  private val intertokenSpace = atmosphere.*
  private val identifier      = initial ~ m(subsequent)(`*`) ^^ { _.toString }
  private val variable        = identifier
  private val specialInitial = "!".r | "\\$".r | "%".r | "~".r | "\\*".r | "/".r | ":".r | "<".r | "=".r | ">".r |
    "\\?".r | "\\^".r | "_".r | "&".r

  private val letter  = "[a-z]".r
  private val initial = specialInitial | letter
  private val digit   = "0".r | "1".r | "2".r | "3".r | "4".r | "5".r | "6".r | "7".r | "8".r | "9".r

  private val subsequent = initial | digit | specialSubsequent

  private val specialSubsequent = "\\+".r | "-".r | "\\.".r | "@".r
  private val expressionKeyword = "quote".r | "lambda".r | "if".r | "set!".r | "begin".r | "cond".r | "and".r | "or".r | "case".r | "let".r |
    "letrec".r | "do".r | "delay".r | "quasiquote".r

  private val syntacticKeyword = "else".r | "=>".r | "define".r | "unquote".r | "unquote-splicing".r | expressionKeyword

  private val boolean                            = "True".r | "False".r
  private val character                          = "\'.\'".r
  private val string                             = """"[^"]*"""".r
  private val number                             = "^-?\\d+$".r | simpleFloatReg | scientificNotationFloatReg
  private val simpleFloatReg             = "^[+-]?([0-9]*)?\\.[0-9]+$".r
  private val scientificNotationFloatReg = "-?[\\d.]+(?:E-?\\d+)?".r

  private val lp: Parser[AnyValToken] = "\\(".r ^^ identity
  private val rp: Parser[AnyValToken] = "\\)".r ^^ identity

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

  def apply(): SchemeLexer = new SchemeLexer()
}

class SchemeLexer extends RegexParsers with Lexer[AnyValToken] {

  def dosth() = {
    parse(program, "assad")
  }

//  override def lex(code: String) = parse
}
