package parser

import util.ParseSpec

class ParserAnonFuncTest extends ParseSpec {
  it should "parse no arg anonymous function correctly" in {
    parser.parse("(lambda () ())") shouldBe LAMBDA(
      List(),
      List()
    )
  }

  it should "parse one arg anonymous function correctly" in {
    parser.parse("(lambda (x) (+ 2 3))") shouldBe LAMBDA(
      List(ID("x")),
      List(ID("+"), INT(2), INT(3))
    )
  }

  it should "parse two arg anonymous function correctly" in {
    parser.parse("(lambda (x y) (+ 2 3 ))") shouldBe LAMBDA(
      List(ID("x"), ID("y")),
      List(ID("+"), INT(2), INT(3))
    )
  }

  it should "parse three arg anonymous function correctly" in {
    parser.parse("(lambda (x y z) (+ 2 3 ))") shouldBe LAMBDA(
      List(ID("x"), ID("y"), ID("z")),
      List(ID("+"), INT(2), INT(3))
    )
  }

  it should "parse four arg anonymous function correctly" in {
    parser.parse("(lambda (a b c d) (+ 2 3 ))") shouldBe LAMBDA(
      List(ID("a"), ID("b"), ID("c"), ID("d")),
      List(ID("+"), INT(2), INT(3))
    )
  }
}
