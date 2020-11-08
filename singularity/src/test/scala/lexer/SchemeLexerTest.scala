package lexer

import org.scalatest.flatspec.AnyFlatSpec

class SchemeLexerTest extends AnyFlatSpec {
  val lexer = new SchemeLexer()

  it should "run" in {

    val defineLambdaCode =
      """
        |(define square
        |  (lambda (n)
        |    (* n n)))
        |""".stripMargin

    val res = lexer(defineLambdaCode)
    println(res)
  }
}
