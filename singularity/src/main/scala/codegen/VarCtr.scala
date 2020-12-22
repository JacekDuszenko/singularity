package codegen

import config.SETTINGS.GeneratedVariablePrefix

object VarCtr {
  private var c = 0

  def uniqVarName: String = {
    c += 1
    s"$GeneratedVariablePrefix$c"
  }
}
