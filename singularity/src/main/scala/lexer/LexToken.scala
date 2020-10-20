package lexer

trait LexToken[A] {

  val scalaVal: A
}
