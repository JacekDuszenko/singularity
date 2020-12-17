package codegen

import cats.data.State
import codegen.CodegenHelpers.makeClass
import javassist.CtNewMethod

object TypeSystemInitializer {

  private def initializerClassGeneratorState: State[Context, String] = State[Context, String] {
    ctx =>
      val (clazz, newCtx) = makeClass("Main", ctx)
      clazz.addMethod(
        CtNewMethod.make(
          "public static void main (String [] args) {}",
          clazz
        )
      )
      (newCtx, "initialized main class")
  }
}
