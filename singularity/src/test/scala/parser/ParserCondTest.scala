package parser

import util.ParseSpec

class ParserCondTest extends ParseSpec {

  it should "parse cond successfully" in {
    parser.parse("""(cond [#t 2] [#f (lambda (x) (+ 2 x))] [(list 3 5 6) 'h'] )
        |(cond [#t 2] [#f (lambda (x) (+ 2 x))] [(list 3 5 6) 'h'] )
        |""".stripMargin) shouldBe
      List(
        COND(
          List(
            (BOOL(true), INT(2)),
            (BOOL(false), LAMBDA(List(ID("x")), LIST(List(ID("+"), INT(2), ID("x"))))),
            (LISTCONS(List(INT(3), INT(5), INT(6))), CHAR('h'))
          )
        ),
        COND(
          List(
            (BOOL(true), INT(2)),
            (BOOL(false), LAMBDA(List(ID("x")), LIST(List(ID("+"), INT(2), ID("x"))))),
            (LISTCONS(List(INT(3), INT(5), INT(6))), CHAR('h'))
          )
        )
      )
  }
}
