package codegen.creator.impl

import codegen.CodegenHelpers.{getCls, getMthd}
import codegen.creator.Creator
import codegen.{Context, VarCtr}
import javassist.CtMethod
import model._

final case class AppCreator(
    ctx:           Context,
    args:          List[Token[_]],
    anonClassName: String,
    execCls:       String,
    execMtd:       String
) extends Creator {
  override def handle = ???

  def handleLambdaApp(): (Context, String) = {
    val lambdaVar = VarCtr.uniqVarName
    m.addLocalVariable(lambdaVar, getCls(anonClassName))
    m.insertAfter(s"""$lambdaVar = new $anonClassName();""")
    m.insertAfter(s"""$lambdaVar.call(${evalAppArgs(args).mkString(",")});""")
    (ctx, "added lambda app")
  }

  private def evalAppArgs(args: List[Token[_]]): List[String] = {
    args.map {
      case ID(str)     => str
      case STRING(str) => s""" "$str" """
      case BOOL(b)     => if (b) "true" else "false"
      case INT(num)    => s"$num"
      case CHAR(c)     => s"'$c'"
      case requiresEvaluation =>
        print(s"not implemented YET for token $requiresEvaluation"); "NOTIMPLEMENTED"
    }
  }
  private def m: CtMethod = getMthd(execCls, execMtd)
}
