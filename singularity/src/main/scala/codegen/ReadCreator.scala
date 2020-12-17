package codegen

import codegen.CodegenHelpers.getMain
import codegen.VariableType.STRING
import javassist.ClassPool

class ReadCreator(currentContext: Context, variableId: String) {

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
