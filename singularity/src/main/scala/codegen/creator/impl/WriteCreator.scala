package codegen.creator.impl

import codegen.CodeGenerator.{generateDefinedFunApp, generateLambdaApp}
import codegen.CodegenHelpers.getMain
import codegen.Context
import codegen.creator.Creator
import codegen.creator.impl.WriteCreator.handlePrint
import javassist.CtMethod
import model._

case class WriteCreator(ctx: Context, expr: Token[_]) extends Creator {
  override def handle: (Context, String) = handlePrint(getMain, expr, insideLambda = false, ctx)
}

object WriteCreator {
  def handlePrint(
      methodToHandle: CtMethod,
      expr:           Token[_],
      insideLambda:   Boolean = false,
      context:        Context
  ): (Context, String) = {
    val mtdName   = methodToHandle.getName
    val clazzName = methodToHandle.getDeclaringClass.getName
    expr match {
      case ID(_) if insideLambda => prt(methodToHandle, "$1", context)
      case ID(str)               => prt(methodToHandle, str, context)
      case STRING(str)           => prt(methodToHandle, s""" "$str" """, context)
      case BOOL(b)               => prt(methodToHandle, if (b) "true" else "false", context)
      case INT(num)              => prt(methodToHandle, s"$num", context)
      case CHAR(c)               => prt(methodToHandle, s"'$c'", context)
      case LIST((lambda @ LAMBDA(_, _)) :: tail) =>
        val (nctx, insidePrint) = generateLambdaApp(clazzName, mtdName, context, lambda, tail)
        prt(methodToHandle, insidePrint, nctx)
      case LIST(ID(varName) :: tail) =>
        val (nctx, insidePrint) = generateDefinedFunApp(clazzName, mtdName, context, varName, tail)
        prt(methodToHandle, insidePrint, nctx)
      case IF(cond: Token[_], positive: Token[_], negative: Token[_]) =>
        val (nctx, insidePrint) = generateDefinedFunApp(
          clazzName,
          mtdName,
          context,
          "if",
          cond :: positive :: negative :: Nil
        )
        prt(methodToHandle, insidePrint, nctx)
      case requiresEvaluation =>
        print(s"not implemented YET for token $requiresEvaluation");
        (context, "not implemented yet")
    }
  }

  def prt(method: CtMethod, codeInside: String, ctx: Context): (Context, String) = {
    method.insertAfter(s"""System.out.println($codeInside);""")
    (ctx, codeInside)
  }
}
