package config

import model._

object SETTINGS {
  val CompileOutputDir: String        = "WYNIK_KOMPILACJI"
  val GeneratedVariablePrefix: String = "gen$var"

  val NativeOperators: Map[String, NativeOperator] =
    Map(
      "+"                 -> PLUS,
      "-"                 -> SUB,
      "*"                 -> MUL,
      "/"                 -> DIV,
      "JVM.String.length" -> JVMSTRING,
      "%"                 -> MOD,
      "and"               -> AND,
      "or"                -> OR,
      "="                 -> EQ,
      "!="                -> NEQ,
      "if"                -> IFOP,
      "<="                -> LEQ
    )
}
