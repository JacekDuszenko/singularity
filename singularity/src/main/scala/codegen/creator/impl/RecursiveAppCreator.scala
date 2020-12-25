package codegen.creator.impl

import codegen.CodeGenerator.{generateDefinedFunApp, generateLambdaApp}
import codegen.CodegenHelpers.{getCls, getMthd}
import codegen.creator.Creator
import codegen.{Context, VarCtr}
import model.{BOOL, CHAR, ID, INT, LAMBDA, LIST, STRING, Token}

case class RecursiveAppCreator(
    execCls: String,
    execMtd: String,
    ctx:     Context,
    varName: String,
    tail:    List[Token[_]]
) extends Creator {

  override def handle: (Context, String) = {
    val lambdaVar = VarCtr.uniqVarName
    val lambdaClz = ctx.scope(varName).anonClassName
    m.addLocalVariable(lambdaVar, getCls(lambdaClz))
    m.insertAfter(s"""$lambdaVar = new $lambdaClz();""")
    val (nctx, argz) = evalAppArgs(tail)
    m.insertAfter(s"""$lambdaVar.call(${argz.mkString(",")});""")
    (nctx, s"$lambdaVar.getValue()")
  }

  def evalAppArgs(toks: List[Token[_]]): (Context, List[String]) =
    toks.foldLeft((ctx, List[String]()))((acc: (Context, List[String]), token: Token[_]) => {
      val (nc, nvar) = eval(token, acc._1)
      (nc, acc._2 :+ nvar)
    })

  def eval(
      tkn: Token[_],
      ctx: Context
  ): (Context, String) = tkn match {
    case ID(str)     => (ctx, ctx.scope(str).name)
    case STRING(str) => (ctx, s""" new String("$str") """)
    case BOOL(b)     => (ctx, if (b) "new Boolean(true)" else "new Boolean(false)")
    case INT(num)    => (ctx, s"new Integer($num)")
    case CHAR(c)     => (ctx, s"new Character('$c')")
    case LIST((lambda @ LAMBDA(_, _)) :: tail) =>
      generateLambdaApp(execCls, execMtd, ctx, lambda, tail)
    case LIST(ID(varName) :: tail) =>
      generateDefinedFunApp(execCls, execMtd, ctx, varName, tail)

    case requiresEvaluation => (ctx, "requires evaluation")
  }

  override def m = getMthd(execCls, execMtd)
}
