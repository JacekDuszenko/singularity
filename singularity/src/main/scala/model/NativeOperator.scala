package model

import codegen.VariableType
import codegen.VariableType._
import javassist.CtClass

sealed trait NativeOperator {
  def requiredArgTypes: List[VariableType]
  def getResultType: (String, String)
  def argLen: Int
  def syntax: String
  val formatArgs: (String, String) => String
}

case object PLUS extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "+"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#add(($fst), ($snd) )"""
}

case object MUL extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "*"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#mul(($fst), ($snd) )"""
}

case object DIV extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "/"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#div(($fst), ($snd) )"""
}

case object SUB extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "-"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#sub(($fst), ($snd) )"""
}

case object MOD extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "%"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#mod(($fst), ($snd) )"""
}

case object EQ extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "="

  override def getResultType = ("Boolean", "java.lang.Boolean")

  override val formatArgs = (fst, snd) => s"""($fst.equals($snd))"""
}

case object NEQ extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "!="

  override def getResultType = ("Boolean", "java.lang.Boolean")

  override val formatArgs = (fst, snd) => s"""(! $fst.equals($snd))"""
}
