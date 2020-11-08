package lexer

import org.scalatest.flatspec.AnyFlatSpec

class SchemeLexerTest extends AnyFlatSpec {
  val lexer = SchemeLexer

  it should "run1" in {

    val defineLambdaCode =
      """
        |(define square
        |  (lambda (n)
        |    (* n n)))
        |""".stripMargin

    val res = lexer(defineLambdaCode)
    println(res)
  }

  it should "run2" in {

    val defineLambdaCode =
      """
        | "hello, world"
        |""".stripMargin

    val res = lexer(defineLambdaCode)
    println(res.get)
  }

  it should "run3" in {

    val defineLambdaCode =
      """
        | "(+ (+ 2 2) (+ 2 2))"
        |""".stripMargin

    val res = lexer(defineLambdaCode)
    println(res.get)
  }

  it should "run4" in {

    val defineLambdaCode =
      """
        |(if True False True)
        |(or True "XDXDXDXD" 'a')
        |""".stripMargin

    val res = lexer(defineLambdaCode)
    println(res.get)
  }

  it should "run5" in {

    val defineLambdaCode =
      """
        | (funkcja (3 4 5))
        |""".stripMargin

    val res = lexer(defineLambdaCode)
    println(res.get)
  }
}
