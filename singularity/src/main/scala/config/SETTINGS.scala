package config

import model._

object SETTINGS {
  val CompileOutputDir: String        = "WYNIK_KOMPILACJI"
  val GeneratedVariablePrefix: String = "gen$var"

  val NativeOperators: Map[String, NativeOperator] =
    Map(
      "+"      -> PLUS,
      "-"      -> SUB,
      "*"      -> MUL,
      "/"      -> DIV,
      "length" -> null,
      "%"      -> null,
      "and"    -> null, //TODO implement
      "or"     -> null,
      "="      -> EQ,
      "!="     -> EQ
    )
}
