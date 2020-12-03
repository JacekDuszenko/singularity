package parser

import scala.util.parsing.combinator.JavaTokenParsers

object SchemeParser extends JavaTokenParsers {
  type AnyValToken = Token[_]

  private def space = regex("[ \\n]*".r)
  private def bool: Parser[Token[Boolean]] =
    ("#t".r | "#f".r) ^^ { case "#t" => BOOL(true); case "#f" => BOOL(false) }

  private def id: Parser[Token[String]] =
    """[a-zA-Z=*+/<>!\?][a-zA-Z0-9=*+/<>!\?]*""".r ^^ { matched =>
      ID(matched)
    }

  private def num: Parser[Token[Int]] = """-?\d+""".r ^^ { s =>
    INT(s.toInt)
  }

  private def str: Parser[Token[String]] = '"' ~> """[^""]*""".r <~ '"' <~ space ^^ { s =>
    STRING(s)
  }

  private def list: Parser[LIST] =
    '(' ~> space ~> rep(expr) <~ ')' <~ space ^^ { s: List[Token[_]] =>
      LIST(s)
    }

  def expr: Parser[Token[_]] = id | str | num | bool | list ^^ identity

  def parse(code: String): Any = parse(expr, code).get
}

/*
expr -> literal | id | list
literal -> str | num | bool
 */
