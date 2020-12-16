package parser

import scala.util.parsing.combinator.JavaTokenParsers

object SchemeParser extends JavaTokenParsers {
  type AnyValToken = Token[_]

  private def space = regex("[ \\n]*".r)
  private def bool: Parser[Token[Boolean]] =
    ("#t".r | "#f".r) ^^ { case "#t" => BOOL(true); case "#f" => BOOL(false) }

  private def id: Parser[Token[String]] =
    """[a-zA-Z=*+/<>!\-\?][a-zA-Z0-9=*+/<>!\?]*""".r ^^ { matched =>
      ID(matched)
    }

  private def num: Parser[Token[Int]] = """-?\d+""".r ^^ { s =>
    INT(s.toInt)
  }

  private def str: Parser[Token[String]] = '"' ~> """[^""]*""".r <~ '"' <~ space ^^ { s =>
    STRING(s)
  }

  private def list: Parser[LIST] =
    '(' ~> space ~> rep(expr) <~ space <~ ')' <~ space ^^ { s: List[Token[_]] =>
      LIST(s)
    }

  private def lambda: Parser[LAMBDA] = {
    val args  = '(' ~> space ~> "lambda" ~> space ~> '(' ~> space ~> rep(id) <~ space <~ ')'
    val exprs = space ~> list <~ ')'
    args ~ exprs ^^ { arg =>
      LAMBDA(arg._1, arg._2.scalaVal)
    }
  }

  private def listCons: Parser[LISTCONS] =
    inparens {
      "list" ~> space ~> rep(expr) <~ space
    } ^^ {
      case (tokenz) => LISTCONS(tokenz)
    }

  private def `if`: Parser[IF] =
    inparens {
      "if" ~> space ~> (spcd(expr) ~ spcd(expr) ~ spcd(expr))
    } ^^ {
      case a ~ b ~ c => IF(a, b, c)
    }

  private def cond: Parser[COND] =
    inparens {
      "cond" ~> space ~> rep()
    }

  private def spcd[A](parser: Parser[A]) =
    space ~> parser <~ space

  private def inparens[A](parser: Parser[A], lp: Char = '(', rp: Char = ')') =
    space ~> lp ~> space ~> parser <~ space <~ rp <~ space

  private val keywordExpr = lambda | `if` | listCons
  private def userDefExpr = list | str | num | bool | id

  private def expr: Parser[Token[_]] = keywordExpr | userDefExpr ^^ identity

  def parse(code: String): Any = parse(expr, code).get
}

/*
expr -> literal | id | list
literal -> str | num | bool
 */
