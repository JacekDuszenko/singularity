package codegen.creator.impl

import codegen.Context
import codegen.creator.Creator
import javassist.{ClassPool, CtClass, CtMethod, CtNewMethod}
import model.{ID, Token, WRITE}

case class LambdaCreator(ctx: Context, vars: List[Token[_]], expr: Token[_]) extends Creator {
  def mkAnonClass(length: Int): (CtClass, Context) = {
    val parent  = ClassPool.getDefault.get(s"Fun$length")
    val clzName = s"Fun${length}$$Anon${ctx.anonCtr}"
    val clz     = ClassPool.getDefault.makeClass(clzName)
    clz.setSuperclass(parent)
    val lambdaAddedCtx =
      ctx.copy(anonCtr = ctx.anonCtr + 1, classnamesToWrite = clzName :: ctx.classnamesToWrite)
    val mthd   = CtNewMethod.make(s"public void call(${toArgList(vars).mkString(",")}) {}", clz)
    val newCtx = generateLambdaBody(mthd, lambdaAddedCtx, toArgList(vars), expr)
    clz.addMethod(mthd)

    (clz, newCtx)
  }

  private def toArgList(vars: List[Token[_]]): List[String] = vars.map {
    case ID(s) => s"Object $s"
  }

  override def handle: (Context, String) =
    (mkAnonClass(vars.length)._2, "generated lambda template")

  def generateLambdaBody(
      mtd:  CtMethod,
      ctx:  Context,
      vars: List[String],
      expr: Token[_]
  ): Context = {
    expr match {
      case WRITE(expr) => WriteCreator.handlePrint(mtd, expr)
    }
    ctx
  }
}
