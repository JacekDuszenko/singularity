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

  override val formatArgs = (fst, snd) => s"""Ops#add(((Integer)$fst), ((Integer)$snd) )"""
}

case object MUL extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "*"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#mul(((Integer)$fst), ((Integer)$snd) )"""
}

case object DIV extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "/"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#div(((Integer)$fst), ((Integer)$snd) )"""
}

case object SUB extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "-"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#sub(((Integer)$fst), ((Integer)$snd) )"""
}

case object MOD extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "%"

  override def getResultType = ("Integer", "java.lang.Integer")

  override val formatArgs = (fst, snd) => s"""Ops#mod(((Integer)$fst), ((Integer)$snd) )"""
}

case object EQ extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "="

  override def getResultType = ("Boolean", "java.lang.Boolean")

  override val formatArgs = (fst, snd) => s"""Ops#eq($fst, $snd)"""
}

case object NEQ extends NativeOperator {
  override def requiredArgTypes = List(NUM, NUM)

  override def argLen = 2

  override def syntax = "!="

  override def getResultType = ("Boolean", "java.lang.Boolean")

  override val formatArgs = (fst, snd) => s"""Ops#neq($fst, $snd)"""
}

case object IFOP extends NativeOperator {
  override def requiredArgTypes = List(UNKNOWN, UNKNOWN, UNKNOWN)

  override def getResultType = ("Object", "java.lang.Object")

  override def argLen = 3

  override def syntax = "if"

  override val formatArgs = (fst, snd) => s"""irrelevant"""
}
