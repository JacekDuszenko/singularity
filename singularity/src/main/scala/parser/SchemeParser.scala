package parser

import model._

import scala.util.chaining._
import scala.util.parsing.combinator.JavaTokenParsers

object SchemeParser extends JavaTokenParsers {
  type AnyValToken = Token[_]

  private def space = regex("[ \\n]*".r)
  private def bool: Parser[Token[Boolean]] =
    ("#t".r | "#f".r) ^^ { case "#t" => BOOL(true); case "#f" => BOOL(false) }

  private def id: Parser[Token[String]] =
    """[a-zA-Z=*%+/<>!\-\?][a-zA-Z0-9=*+/<>!\?\.]*""".r ^^ { matched =>
      ID(matched)
    }

  private def num: Parser[Token[Int]] = """-?\d+""".r ^^ { s =>
    INT(s.toInt)
  }

  private def str: Parser[Token[String]] = spcd('"' ~> """[^""]*""".r <~ '"') ^^ { s =>
    STRING(s)
  }

  private def char: Parser[Token[Char]] = spcd('\'' ~> ".".r <~ '\'') ^^ { e =>
    CHAR(e.charAt(0))
  }

  private def list: Parser[LIST] =
    spcd(inparens(spcd(rep(expr)))) ^^ { s: List[Token[_]] =>
      LIST(s)
    }

  private def lambda: Parser[LAMBDA] = {
    val args       = '(' ~> space ~> "lambda" ~> space ~> '(' ~> space ~> rep(id) <~ space <~ ')'
    val expression = space ~> expr <~ space <~ ')'
    args ~ expression ^^ { arg =>
      LAMBDA(arg._1, arg._2)
    }
  }

  private def let: Parser[LIST] =
    inparens {
      "let" ~> space ~> inparens {
        spcd(rep(inparens(spcd(id) ~ spcd(expr), lp = '[', rp = ']')))
      } ~ spcd(expr)
    } ^^ {
      case (bindings @ _) ~ (expression @ _) =>
        LIST(List(LAMBDA(bindings.map(_._1), expression)) ++ bindings.map(_._2))
    }

  private def listCons: Parser[LISTCONS] =
    inparens {
      "list" ~> space ~> rep(expr) <~ space
    } ^^ (tokenz => LISTCONS(tokenz))

  private def `if`: Parser[IF] =
    inparens {
      "if" ~> space ~> (spcd(expr) ~ spcd(expr) ~ spcd(expr))
    } ^^ {
      case a ~ b ~ c => IF(a, b, c)
    }

  private def cond: Parser[COND] =
    inparens {
      "cond" ~> spcd(rep(inparens(spcd(expr) ~ spcd(expr), lp = '[', rp = ']')))
    } ^^ {
      case l @ _ => l.map(tks => (tks._1, tks._2)) pipe COND
    }

  private def write: Parser[WRITE] =
    inparens {
      "write" ~> spcd(expr)
    } ^^ (xpr => WRITE(xpr))

  private def spcd[A](parser: Parser[A]) =
    space ~> parser <~ space

  private def inparens[A](parser: Parser[A], lp: Char = '(', rp: Char = ')') =
    space ~> lp ~> space ~> parser <~ space <~ rp <~ space

  private val keywordExpr = lambda | `if` | listCons | cond | let | write
  private def userDefExpr = list | str | char | num | bool | id

  private def expr: Parser[Token[_]] = keywordExpr | userDefExpr ^^ identity

  private def readDef: Parser[READDEF] =
    inparens("define" ~> spcd(id) <~ "read") ^^ (a => READDEF(ID(a.scalaVal)))

  private def regularDef: Parser[DEF] = inparens("define" ~> spcd(id) ~ spcd(expr)) ^^ {
    case arg ~ ex => DEF(ID(arg.scalaVal), ex)
  }

  private def definition: Parser[Token[_]] = (readDef | regularDef) ^^ identity

  private def form = definition | expr

  private def program = rep(spcd(form))

  def parse(code: String): List[Token[_]] = parse(program, code).get
}
