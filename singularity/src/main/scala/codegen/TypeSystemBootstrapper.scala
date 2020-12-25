package codegen

import cats.data.State
import codegen.CodegenHelpers.{makeArgList, makeClass}
import javassist.{ClassPool, CtField, CtNewConstructor, CtNewMethod}

object TypeSystemBootstrapper {

  def initializeTypeSystemBuilder: State[Context, String] =
    for {
      _ <- addMainClass()
      _ <- addValueInterface()
      _ <- addPrimitiveTypes()
      - <- addFuncTypes()
      _ <- addOps()
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

  private def addOps(): State[Context, String] = State[Context, String] { ctx =>
    val (clazz, newCtx) = makeClass("Ops", ctx)
    clazz.addMethod(
      CtNewMethod.make(
        """public static Integer add(Integer a, Integer b) {
        |   return new java.lang.Integer(a.intValue() + b.intValue());
        |}"""".stripMargin,
        clazz
      )
    )

    clazz.addMethod(
      CtNewMethod.make(
        """public static Integer mul(Integer a, Integer b) {
        |   return new java.lang.Integer(a.intValue() * b.intValue());
        |}"""".stripMargin,
        clazz
      )
    )

    clazz.addMethod(
      CtNewMethod.make(
        """public static Integer sub(Integer a, Integer b) {
        |   return new java.lang.Integer(a.intValue() - b.intValue());
        |}"""".stripMargin,
        clazz
      )
    )

    clazz.addMethod(
      CtNewMethod.make(
        """public static Integer div(Integer a, Integer b) {
        |   return new java.lang.Integer(a.intValue() / b.intValue());
        |}"""".stripMargin,
        clazz
      )
    )

    clazz.addMethod(
      CtNewMethod.make(
        """public static Integer mod(Integer a, Integer b) {
          |   return new java.lang.Integer(a.intValue() % b.intValue());
          |}"""".stripMargin,
        clazz
      )
    )

    clazz.addMethod(CtNewMethod.make("""public static Boolean eq(Object a, Object b) {
        |   return new Boolean(a.equals(b));
        |}"""".stripMargin, clazz))

    clazz.addMethod(CtNewMethod.make("""public static Boolean neq(Object a, Object b) {
        |   return new Boolean(!a.equals(b));
        |}"""".stripMargin, clazz))

    clazz.addMethod(
      CtNewMethod.make(
        """public static Integer strlen(Object a) {
                                       |   return new Integer(((String)a).length());
                                       |}"""".stripMargin,
        clazz
      )
    )

    clazz.addMethod(
      CtNewMethod.make(
        """public static Boolean leq(Object a, Object b) {
                                       |   return new Boolean(((Integer)a).intValue() <= ((Integer)b).intValue());
                                       |}"""".stripMargin,
        clazz
      )
    )
    (newCtx, "added ops")

  }

  private def addValueInterface(): State[Context, String] = State[Context, String] { ctx =>
    val iface = ClassPool.getDefault.makeInterface("Value")
    iface.addMethod(CtNewMethod.make("Object getValue();", iface))
    val newCtx = ctx.copy(classnamesToWrite = "Value" :: ctx.classnamesToWrite)
    (newCtx, "added Value interface")
  }

  private def addPrimitiveTypes(): State[Context, String] =
    for {
      _ <- addPrimitiveType("Str", "String")
      _ <- addPrimitiveType("Num", "Integer")
      _ <- addPrimitiveType("Logic", "Boolean")
      _ <- addPrimitiveType("Char", "Character")
    } yield "added primitive types"

  private def addPrimitiveType(name: String, correspondingType: String): State[Context, String] =
    State[Context, String] { ctx =>
      val (clazz, newCtx) = makeClass(name, ctx)
      clazz.addField(CtField.make(s"public final $correspondingType value;", clazz))
      clazz.addInterface(ClassPool.getDefault.get("Value"))
      clazz.addMethod(CtNewMethod.make("public Object getValue() {return this.value;}", clazz))
      clazz.addConstructor(
        CtNewConstructor.make(s"public $name($name value) {this.value = value;}", clazz)
      )
      (newCtx, s"initialized $name type")
    }

  private def addFuncTypes(): State[Context, String] = State[Context, String] { ctx =>
    (List(0, 1, 2, 3, 4).foldLeft(ctx)((ct, num) => addFuncType(num, ct)), "added func types")
  }

  private def addFuncType(argsNumber: Int, ctx: Context): Context = {
    val (clazz, newCtx) = makeClass(s"Fun$argsNumber", ctx)
    clazz.addInterface(ClassPool.getDefault.get("Value"))
    clazz.addField(CtField.make("public Object value;", clazz))
    clazz.addConstructor(CtNewConstructor.defaultConstructor(clazz))
    clazz.addMethod(CtNewMethod.make("public Object getValue() {return this.value;}", clazz))
    clazz.addMethod(
      CtNewMethod.make(
        s"""public String toString() {return "<Function type with $argsNumber args>";}""",
        clazz
      )
    )
    clazz.addMethod(
      CtNewMethod.make(s"public void call(${makeArgList(argsNumber)}) {}", clazz)
    )
    newCtx
  }
}
