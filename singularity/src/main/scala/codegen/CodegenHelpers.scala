package codegen

import javassist.{ClassPool, CtClass, CtMethod}

object CodegenHelpers {

  def getMain: CtMethod = ClassPool.getDefault.getMethod("Main", "main")

  def makeClass(classname: String, ctx: Context): (CtClass, Context) = {
    (
      ClassPool.getDefault.makeClass(classname),
      ctx.copy(classnamesToWrite = classname :: ctx.classnamesToWrite)
    )
  }

  def makeArgList(argNumber: Int): String = {
    if (argNumber == 0) ""
    else
      Range.inclusive(1, argNumber).map(n => s"Object v$n").mkString(",")
  }
}
