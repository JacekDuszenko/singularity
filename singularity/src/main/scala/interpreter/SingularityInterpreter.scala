package interpreter

import parser.ExprParser

/**
  * Main entrypoint to the language interpreter
  */
class SingularityInterpreter {

  def interpret(code: String): Unit = {
    val p: ExprParser = ExprParser

    p.parseAll(p.simpleExpr, "")
  }
}
