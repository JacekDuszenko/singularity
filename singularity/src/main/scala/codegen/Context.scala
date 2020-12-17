package codegen

import enumeratum._

final case class Context(scope: Map[String, VariableMetadata], classnamesToWrite: List[String])

case class VariableMetadata(name: String, `type`: VariableType)

sealed trait VariableType extends EnumEntry

object VariableType extends Enum[VariableType] {
  val values = findValues

  case object NUM    extends VariableType
  case object CHAR   extends VariableType
  case object BOOL   extends VariableType
  case object STRING extends VariableType
  case object HLIST  extends VariableType
  case object FUN0   extends VariableType
  case object FUN1   extends VariableType
  case object FUN2   extends VariableType
  case object FUN3   extends VariableType
  case object FUN4   extends VariableType
}
