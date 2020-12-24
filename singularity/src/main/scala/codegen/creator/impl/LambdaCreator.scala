package codegen.creator.impl

import codegen.CodeGenerator.{generateDefinedFunApp, generateLambdaApp}
import codegen.CodegenHelpers.getObjCls
import codegen.creator.Creator
import codegen.{Context, VarCtr, VariableMetadata, VariableType}
import javassist.{ClassPool, CtMethod, CtNewConstructor, CtNewMethod}
import model._

final case class LambdaCreator(ctx: Context, vars: List[Token[_]], expr: Token[_]) extends Creator {
  def mkAnonClass(length: Int): (String, Context) = {
    val parent  = ClassPool.getDefault.get(s"Fun$length")
    val clzName = s"Fun${length}Anon${ctx.anonCtr}"
    val clz     = ClassPool.getDefault.makeClass(clzName)
    clz.setSuperclass(parent)
    clz.addConstructor(CtNewConstructor.defaultConstructor(clz))
    val lambdaAddedCtx =
      ctx.copy(anonCtr = ctx.anonCtr + 1, classnamesToWrite = clzName :: ctx.classnamesToWrite)
    val mthd =
      CtNewMethod.make(s"public void call(${toArgList(vars).mkString(",")}) {return;}", clz)
    clz.addMethod(mthd)
    val newCtx = generateLambdaBody(mthd, lambdaAddedCtx, toNamesList(vars), expr)

    (clzName, newCtx)
  }

  private def toNamesList(vars: List[Token[_]]): List[String] = vars.map {
    case ID(s) => s
  }

  private def toArgList(vars: List[Token[_]]): List[String] = vars.map {
    case ID(s) => s"Object $s"
  }

  override def handle: (Context, String) = {
    val (clzName, ctx) = mkAnonClass(vars.length)
    (ctx, clzName)
  }

  private def generateLambdaBody(
      mtd:  CtMethod,
      ctx:  Context,
      vars: List[String],
      expr: Token[_]
  ): Context = {
    val varsAddedCtx = addVarsToCtx(mtd, vars, ctx)
    val handledBodyCtx = expr match {
      case WRITE(expr) => WriteCreator.handlePrint(mtd, expr, insideLambda = true, varsAddedCtx)._1
      case LIST((lambda @ LAMBDA(_, _)) :: tail) =>
        val (nctx, varToSaveName) = generateLambdaApp(mtd, varsAddedCtx, lambda, tail)
        mtd.insertAfter(s"value = $varToSaveName ;")
        nctx
      case LIST(ID(varName) :: tail) =>
        val (nctx, varToSaveName) = generateDefinedFunApp(mtd, varsAddedCtx, varName, tail)
        mtd.insertAfter(s"value = $varToSaveName;")
        nctx
    }
    removeVarsFromCtx(handledBodyCtx, vars)
  }

  def addVarsToCtx(mtd: CtMethod, vars: List[String], ctx: Context): Context =
    vars
      .zip(LazyList.from(1))
      .foldLeft(ctx)((context, tpl) => {
        val (variable, index) = tpl
        val localName         = VarCtr.uniqVarName
        mtd.addLocalVariable(localName, getObjCls)
        mtd.insertBefore(s"$localName = $$$index;")
        context.copy(
          scope = context.scope + (variable -> VariableMetadata(localName, VariableType.UNKNOWN))
        )
      })

  def removeVarsFromCtx(ctxWithVars: Context, varNames: List[String]): Context = {
    varNames.foldLeft(ctxWithVars)((c, v) => c.copy(scope = c.scope.removed(v)))
  }

}
