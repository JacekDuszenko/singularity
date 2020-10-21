package lexer

import scala.util.parsing.combinator.RegexParsers

sealed trait LiteralToken[A] extends LexToken[A]

case class ID(str: String) extends LiteralToken[String] {
  override val scalaVal: String = str
}
case class BOOL(b: Boolean) extends LiteralToken[Boolean] {
  override val scalaVal: Boolean = b
}
case class INT(num: Int) extends LiteralToken[Int] {
  override val scalaVal: Int = num
}
case class FLOAT(fnum: Float) extends LiteralToken[Float] {
  override val scalaVal: Float = fnum
}
case class CHAR(char: Char) extends LiteralToken[Char] {
  override val scalaVal: Char = char
}
case class STRING(str: String) extends LiteralToken[String] {
  override val scalaVal: String = str
}

/**
  * LiteralTokenLexer object represents the namespace for lexing literal tokens in the language
  * parsed according to `token` section in
  * https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_9.html
  */
object LiteralTokenLexer extends RegexParsers with Lexer[LiteralToken[_]] {
  override def skipWhitespace: Boolean = true
  override def apply()                 = lit

  def lit = bool | int | float | char | str

  override def lex(code: String) = parse(lit, code)

  private def id: Parser[ID] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str =>
    ID(str)
  }

  private def bool: Parser[BOOL] = "#t|#f".r ^^ { str =>
    if (str == "#t") BOOL(true) else BOOL(false)
  }

  private def int: Parser[INT] = "^-?\\d+$".r ^^ { strnum =>
    INT(strnum.toInt)
  }

  private def float: Parser[FLOAT] = (simpleFloatReg | scientificNotationFloatReg) ^^ { strflt =>
    FLOAT(strflt.toFloat)
  }
  
  private val simpleFloatReg = "^[+-]?([0-9]*)?\\.[0-9]+$".r
  private val scientificNotationFloatReg = "-?[\\d.]+(?:E-?\\d+)?".r

  private def char: Parser[CHAR] = "\'.\'".r ^^ { strchar =>
    val singleChar = strchar.replace("'", "")
    if (singleChar.length == 1) CHAR(singleChar.charAt(0))
    else throw new RuntimeException("char parsing failed")
  }

  private def str: Parser[STRING] = """"[^"]*"""".r ^^ { str =>
    val content = str.substring(1, str.length - 1)
    STRING(content)
  }
}
