package codegen.creator.impl

import codegen.CodegenHelpers.getMain
import codegen.VariableType.STRING
import codegen.creator.Creator
import codegen.{Context, VariableMetadata}
import javassist.ClassPool

final class ReadCreator(currentContext: Context, variableId: String) extends Creator {

  def handle: (Context, String) = {
    handleCodegen()
    handleStateUpdate()
  }

  private def handleCodegen(): Unit = {
    getMain.addLocalVariable(variableId, ClassPool.getDefault.get("Str"))
    getMain.insertAfter(s"$variableId = new java.util.Scanner(System.in).nextLine();")
  }

  private def handleStateUpdate(): (Context, String) = {
    (
      currentContext.copy(
        scope = currentContext.scope + (variableId -> VariableMetadata(variableId, STRING))
      ),
      "new read variable"
    )
  }
}
