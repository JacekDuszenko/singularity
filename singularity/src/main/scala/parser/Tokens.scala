package parser

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
