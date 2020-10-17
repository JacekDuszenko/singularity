package parser

sealed trait LetRec extends ParseTrait


class LetRecParser extends LetRec {

  override def parse(code: String): String = ???
}