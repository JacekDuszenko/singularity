package codegen.creator.impl

import codegen.CodeGenerator.{generateDefinedFunApp, generateLambdaApp}
import codegen.CodegenHelpers.getMthd
import codegen.creator.Creator
import codegen.{Context, VarCtr}
import config.SETTINGS.NativeOperators
import javassist.ClassPool
import model.{BOOL, STRING, _}

final case class NativeAppCreator(
    execCls: String,
    execMtd: String,
    ctx:     Context,
    varName: String,
    tail:    List[Token[_]]
) extends Creator {
  override def handle: (Context, String) = generateNativeOperator()

  private def generateNativeOperator(
      ): (Context, String) = {
    val op                = NativeOperators(varName)
    val (_, ctReturnType) = op.getResultType
    val intermediateVar   = VarCtr.uniqVarName
    m.addLocalVariable(intermediateVar, ClassPool.getDefault.get(ctReturnType))
    op.argLen match {
      case 2 =>
        val fst :: snd :: _ = tail
        val (c1, fstVar)    = evalArg(fst, ctx)
        val (c2, sndVar)    = evalArg(snd, c1)
        val body            = s"""$intermediateVar = ${op.formatArgs(fstVar, sndVar)}; 
          $intermediateVar.hashCode();"""
        m.insertAfter(body)
        (c2, intermediateVar)
      case 3 if op == IFOP =>
        val cond :: pos :: neg :: Nil = tail
        val (c1, condEvalResult)      = evalArg(cond, ctx)
        val (c2, positiveEvalResult)  = evalArg(pos, c1)
        val (c3, negativeEvalResult)  = evalArg(neg, c2)
        val body =
          s""" $intermediateVar = ((Boolean)$condEvalResult).booleanValue() == true ? ($positiveEvalResult) : ($negativeEvalResult);
            | $intermediateVar.hashCode();
            |""".stripMargin
        m.insertAfter(body)
        (c3, intermediateVar)
    }
  }

  private def evalArg(tkn: Token[_], ctx: Context): (Context, String) = tkn match {
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
