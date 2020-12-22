package codegen.creator.impl

import codegen.CodegenHelpers.getMain
import codegen.Context
import codegen.creator.Creator
import codegen.creator.impl.WriteCreator.handlePrint
import javassist.CtMethod
import model._

case class WriteCreator(ctx: Context, expr: Token[_]) extends Creator {
  override def handle = {
    handlePrint(getMain, expr)
    (ctx, "handled write construction")
  }
}

object WriteCreator {
  def handlePrint(methodToHandle: CtMethod, expr: Token[_], lambda: Boolean = false): Unit = {
    expr match {
      case ID(_) if lambda    => prt(methodToHandle, "$1")
      case ID(str)            => prt(methodToHandle, str)
      case STRING(str)        => prt(methodToHandle, s""" "$str" """)
      case BOOL(b)            => prt(methodToHandle, if (b) "true" else "false")
      case INT(num)           => prt(methodToHandle, s"$num")
      case CHAR(c)            => prt(methodToHandle, s"'$c'")
      case requiresEvaluation => print(s"not implemented YET for token $requiresEvaluation")
    }
  }

  def prt(method: CtMethod, codeInside: String): Unit =
    method.insertAfter(s"""System.out.println($codeInside);""")
}
