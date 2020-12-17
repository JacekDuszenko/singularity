package codegen

import cats.data.State
import codegen.CodegenHelpers.makeClass
import javassist.{CtField, CtNewConstructor, CtNewMethod}

object TypeSystemBootstrapper {

  def initializeTypeSystemBuilder: State[Context, String] =
    for {
      _ <- addMainClass()
      _ <- addStringType()
    } yield "initialized type system"

  private def addMainClass() = {
    State[Context, String] { ctx =>
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

  private def addStringType(): State[Context, String] = State[Context, String] { ctx =>
    val (clazz, newCtx) = makeClass("Str", ctx)
    clazz.addField(CtField.make("public final String value;", clazz))
    clazz.addConstructor(
      CtNewConstructor.make("public Str(String value) {this.value = value;}", clazz)
    )
    (newCtx, "initialized string type")
  }
}
