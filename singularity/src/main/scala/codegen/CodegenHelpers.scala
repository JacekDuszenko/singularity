package codegen

import javassist.{ClassPool, CtClass, CtMethod}

object CodegenHelpers {

  def getMain: CtMethod = ClassPool.getDefault.getMethod("Main", "main")

  def makeClass(classname: String, ctx: Context): (CtClass, Context) = {
    (
      ClassPool.getDefault.makeClass(classname),
      ctx.copy(classnamesToWrite = "Main" :: ctx.classnamesToWrite)
    )
  }
}
