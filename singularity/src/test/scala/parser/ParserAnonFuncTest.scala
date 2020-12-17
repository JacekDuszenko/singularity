package parser

import model.{ID, INT, LAMBDA, LIST}
import util.ParseSpec

class ParserAnonFuncTest extends ParseSpec {
  it should "parse no arg anonymous function correctly" in {
    parser.parse("(lambda () ())") shouldBe List(
      LAMBDA(
        List(),
        LIST(List())
      )
    )
  }

  it should "parse one arg anonymous function correctly" in {
    parser.parse("(lambda (x) (+ 2 3))") shouldBe List(
      LAMBDA(
        List(ID("x")),
        LIST(List(ID("+"), INT(2), INT(3)))
      )
    )
  }

  it should "parse two arg anonymous function correctly" in {
    parser.parse("(lambda (x y) (+ 2 3 ))") shouldBe List(
      LAMBDA(
        List(ID("x"), ID("y")),
        LIST(List(ID("+"), INT(2), INT(3)))
      )
    )
  }

  it should "parse three arg anonymous function correctly" in {
    parser.parse("(lambda (x y z) (+ 2 3 ))") shouldBe List(
      LAMBDA(
        List(ID("x"), ID("y"), ID("z")),
        LIST(List(ID("+"), INT(2), INT(3)))
      )
    )
  }

  it should "parse four arg anonymous function correctly" in {
    parser.parse("(lambda (a b c d) (+ 2 3 ))") shouldBe List(
      LAMBDA(
        List(ID("a"), ID("b"), ID("c"), ID("d")),
        LIST(List(ID("+"), INT(2), INT(3)))
      )
    )
  }
}
