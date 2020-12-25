package codegen.creator.impl

import codegen.CodegenHelpers.getCls
import codegen.creator.Creator
import codegen.{Context, VariableType}
import model._

final case class DefineCreator(ctx: Context, variableName: String, expr: Token[_]) extends Creator {
  override def handle: (Context, String) = {

    val (tp, xpr, nctx, _) = evalExpr
    m.addLocalVariable(variableName, getCls(tp))
    m.insertAfter(s"""$variableName = new $xpr;$variableName.hashCode();""")
    (nctx, "generated lmbda body")
  }

  private def evalExpr: (String, String, Context, VariableType) = expr match {
    case ID(_) =>
      throw new RuntimeException("assignment of a variable is impossible in the language")
    case STRING(str) => ("java.lang.Object", s"""String("$str") """, ctx, VariableType.STRING)
    case BOOL(b) =>
      ("java.lang.Object", if (b) "Boolean(true)" else "new Boolean(false)", ctx, VariableType.BOOL)
    case INT(num) => ("java.lang.Object", s"Integer($num)", ctx, VariableType.NUM)
    case CHAR(c)  => ("java.lang.Object", s"Character('$c')", ctx, VariableType.CHAR)
    case LAMBDA(vars, expr) =>
      val (anonClassName, newCtx) =
        LambdaCreator(ctx, vars, expr).mkAnonClass(vars.length, lambdaName = variableName)
      (
        anonClassName,
        s"$anonClassName()",
        newCtx,
        VariableType.funTypeByArgumentsNumber(vars.length)
      )
    case _ =>
      throw new RuntimeException("not evaluated yet")
  }
}
