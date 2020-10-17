package parser

sealed trait IfElse extends ParseTrait


class LetRecParser extends LetRec {

  override def parse(code: String): String = ???
}