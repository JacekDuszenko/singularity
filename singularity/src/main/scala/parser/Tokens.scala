package parser

import parser.HelperTypes.LambdaType

sealed trait Token[A] {
  val scalaVal: A
}

final case class ID(str: String) extends Token[String] {
  override val scalaVal: String = str
}

final case class BOOL(b: Boolean) extends Token[Boolean] {
  override val scalaVal: Boolean = b
}

final case class INT(num: Int) extends Token[Int] {
  override val scalaVal: Int = num
}

final case class FLOAT(fnum: Float) extends Token[Float] {
  override val scalaVal: Float = fnum
}

final case class CHAR(char: Char) extends Token[Char] {
  override val scalaVal: Char = char
}

final case class STRING(str: String) extends Token[String] {
  override val scalaVal: String = str
}

final case class LIST(vals: List[Token[_]]) extends Token[List[Token[_]]] {
  override val scalaVal: List[Token[_]] = vals
}

final case class IF(cond: Token[_], positive: Token[_], negative: Token[_])
    extends Token[(Token[_], Token[_], Token[_])] {
  override val scalaVal = (cond, positive, negative)
}

final case class LAMBDA(vars: List[Token[_]], expr: Token[_]) extends Token[LambdaType] {
  override val scalaVal = (vars, expr)
}

final case class LISTCONS(elems: List[Token[_]]) extends Token[List[Token[_]]] {
  override val scalaVal = elems
}

final case class COND(elems: List[(Token[_], Token[_])]) extends Token[List[(Token[_], Token[_])]] {
  override val scalaVal = elems
}

object HelperTypes {
  type LambdaType = (List[Token[_]], Token[_])
}
