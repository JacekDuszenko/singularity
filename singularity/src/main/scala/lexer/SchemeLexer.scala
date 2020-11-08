package lexer

import lexer.ConstantTokenLexer.{bool, float, int, regex, str}

class SchemeLexer extends Lexer[LexToken[Any]] {

//  val file          = ???
//  val form          = ???
//  val expression    = ???
//  val definition    = ???
//  val constant      = ???
//  val variable      = ???
//  val identifier    = ???
//  val procedureCall = ???
//  val operator      = ???
//  val operand       = ???
//  val operands      = ???
//  val quotation     = ???
//  val datum         = ???
//  val quote         = ???
//  val body          = ???
//  val callPattern   = ???
//  val pattern       = ???
//  val boundVarList  = ???
//  val test          = ???
//  val consequent    = ???
//  val alternate     = ???
//  val condClause    = ???
//  val caseClause    = ???
//  val bindingSpec   = ???
//  val iterationSpec = ???
//
//  val lambda   = ???
//  val `if`     = ???
//  val `ifElse` = ???
//  val cond     = ???
//  val condElse = ???
//  val and      = ???
//  val or       = ???
//  val `case`   = ???
//  val let      = ???
//  val letrec   = ???
//  val rec      = ???
//  val begin    = ???
//  val sequence = ???
//  val `do`     = ???
//  val delay    = ???

  val singleQuote     = """"""".r
  val lp              = "\\(".r
  val rp              = "\\)".r
  val token           = identifier | boolean | number | char | string | lp | rp | ("#".r & lp) | "'".r | "`".r | ",".r | ",@".r | "\\.".r
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
  val char    = """#c""".r ~ ConstantTokenLexer.char
  val string  = str
  val number  = int | float

  override def apply() = ???

  override def lex(code: String) = ???

  override type Elem = this.type
}
