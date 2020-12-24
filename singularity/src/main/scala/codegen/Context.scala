package codegen

import enumeratum._
import javax.lang.model.element.VariableElement

final case class Context(
    scope:             Map[String, VariableMetadata],
    classnamesToWrite: List[String],
    anonCtr:           Int = 1
)

final case class VariableMetadata(
    name:          String,
    `type`:        VariableType,
    anonClassName: String = "unknown"
)

sealed trait VariableType extends EnumEntry

object VariableType extends Enum[VariableType] {
  val values = findValues

  case object NUM     extends VariableType
  case object CHAR    extends VariableType
  case object BOOL    extends VariableType
  case object STRING  extends VariableType
  case object HLIST   extends VariableType
  case object FUN0    extends VariableType
  case object FUN1    extends VariableType
  case object FUN2    extends VariableType
  case object FUN3    extends VariableType
  case object FUN4    extends VariableType
  case object UNKNOWN extends VariableType

  def funTypeByArgumentsNumber(argNo: Int): VariableType = argNo match {
    case 0 => VariableType.FUN0
    case 1 => VariableType.FUN1
    case 2 => VariableType.FUN2
    case 3 => VariableType.FUN3
    case 4 => VariableType.FUN4
  }
}
