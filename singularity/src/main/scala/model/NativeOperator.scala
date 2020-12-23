package model

import codegen.VariableType
import codegen.VariableType._

sealed trait NativeOperator {
  def requiredArgTypes: List[VariableType]
  def returnArgType: VariableType
  def argLen: Int
  def syntax: String
  val formatArgs: (String, String) => String
}

case object PLUS extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "+"

  override def returnArgType = NUM

  override val formatArgs = (fst, snd) => s"""($fst + $snd)"""
}

case object MUL extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "*"

  override def returnArgType = NUM

  override val formatArgs = (fst, snd) => s"""($fst * $snd)"""
}

case object DIV extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "/"

  override def returnArgType = NUM

  override val formatArgs = (fst, snd) => s"""($fst / $snd)"""
}

case object SUB extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "-"

  override def returnArgType = NUM

  override val formatArgs = (fst, snd) => s"""($fst - $snd)"""
}

case object EQ extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "="

  override def returnArgType = BOOL

  override val formatArgs = (fst, snd) => s"""($fst.equals($snd))"""
}

case object NEQ extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "!="

  override def returnArgType = BOOL

  override val formatArgs = (fst, snd) => s"""(! $fst.equals($snd))"""
}
