package codegen.creator.impl

import codegen.CodeGenerator.{generateDefinedFunApp, generateLambdaApp}
import codegen.CodegenHelpers.getMthd
import codegen.creator.Creator
import codegen.{Context, VarCtr}
import config.SETTINGS.NativeOperators
import javassist.{ClassPool, CtMethod}
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
      case 1 if op == JVMSTRING =>
        val fst :: _     = tail
        val (c1, fstVar) = evalArg(fst, ctx)
        val body =
          s"""$intermediateVar = ${op.formatArgs(fstVar, "irrelevant")};$intermediateVar.hashCode();"""
        m.insertAfter(body)
        (c1, intermediateVar)
      case 2 =>
        val fst :: snd :: _ = tail
        val (c1, fstVar)    = evalArg(fst, ctx)
        val (c2, sndVar)    = evalArg(snd, c1)
        val body            = s"""$intermediateVar = ${op.formatArgs(fstVar, sndVar)}; 
          $intermediateVar.hashCode();"""
        m.insertAfter(body)
        (c2, intermediateVar)
      case Int.MaxValue =>
        val opSign = if (op == AND) "&&" else "||"
        val (nctx, evaluatedVarz) =
          tail.foldLeft((ctx, List[String]()))((acc: (Context, List[String]), elem: Token[_]) => {
            val (c, argn) = evalArg(elem, acc._1)
            (c, acc._2 :+ argn)
          })
        val formatted =
          evaluatedVarz.map(v => s""" $v.booleanValue() """).mkString(opSign)
        val body = s"""$intermediateVar = new Boolean($formatted) ;"""
        m.insertAfter(body)
        (nctx, intermediateVar)
      case 3 if op == IFOP =>
        val cond :: pos :: neg :: Nil = tail
        val ifStatementMethodName     = VarCtr.uniqVarName
        val evalCondMethodName        = VarCtr.uniqVarName
        val execPosMethodName         = VarCtr.uniqVarName
        val execNegMethodName         = VarCtr.uniqVarName

        val evalCondMethod = CtMethod.make(s"""public boolean $evalCondMethodName() {
             |return false;
             |}
             |""".stripMargin, m.getDeclaringClass)
        m.getDeclaringClass.addMethod(evalCondMethod)

        val execPosMethod = CtMethod.make(s"""public Object $execPosMethodName() {
             | return null;
             |}
             |""".stripMargin, m.getDeclaringClass)
        m.getDeclaringClass.addMethod(execPosMethod)

        val execNegMethod = CtMethod.make(s"""public Object $execNegMethodName() {
             | return null;
             |}
             |""".stripMargin, m.getDeclaringClass)
        m.getDeclaringClass.addMethod(execNegMethod)

        val (c1, condEvalResult) = evalArg(cond, ctx, execCls, evalCondMethod.getName)
        evalCondMethod.insertAfter(s"""return $condEvalResult.booleanValue();""")

        val (c2, positiveEvalResult) = evalArg(pos, c1, execCls, execPosMethod.getName)
        execPosMethod.insertAfter(s"""return $positiveEvalResult;""")

        val (c3, negativeEvalResult) = evalArg(neg, c2, execCls, execNegMethod.getName)
        execNegMethod.insertAfter(s"""return $negativeEvalResult; """)

        val ifMethod = CtMethod.make(
          s"""public Object $ifStatementMethodName() {
             |    if ($evalCondMethodName()) {
             |      return $execPosMethodName();
             |    } 
             |    else {
             |    return $execNegMethodName();
             |    }
             |}""".stripMargin,
          m.getDeclaringClass
        )
        m.getDeclaringClass.addMethod(ifMethod)

        val body =
          s""" $intermediateVar = $ifStatementMethodName();
            | $intermediateVar.hashCode();
            |""".stripMargin
        m.insertAfter(body)
        (c3, intermediateVar)
    }
  }

  def evalArg(
      tkn: Token[_],
      ctx: Context,
      cls: String = execCls,
      mtd: String = execMtd
  ): (Context, String) = tkn match {
    case ID(str)     => (ctx, ctx.scope(str).name)
    case STRING(str) => (ctx, s""" new String("$str") """)
    case BOOL(b)     => (ctx, if (b) "new Boolean(true)" else "new Boolean(false)")
    case INT(num)    => (ctx, s"new Integer($num)")
    case CHAR(c)     => (ctx, s"new Character('$c')")
    case LIST((lambda @ LAMBDA(_, _)) :: tail) =>
      generateLambdaApp(cls, mtd, ctx, lambda, tail)
    case LIST(ID(varName) :: tail) =>
      generateDefinedFunApp(cls, mtd, ctx, varName, tail)

    case _ => (ctx, "requires evaluation")
  }

  override def m: CtMethod = getMthd(execCls, execMtd)
}

object NativeAppCreator {}
