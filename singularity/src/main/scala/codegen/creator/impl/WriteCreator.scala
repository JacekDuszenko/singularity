package codegen.creator.impl

import codegen.CodegenHelpers.getMain
import codegen.Context
import codegen.creator.Creator
import model._

case class WriteCreator(ctx: Context, expr: Token[_]) extends Creator {
  override def handle = {
    expr match {
      case ID(str)            => prt(str)
      case STRING(str)        => prt(s""" "$str" """)
      case BOOL(b)            => prt(if (b) "true" else "false")
      case INT(num)           => prt(s"$num")
      case CHAR(c)            => prt(s"'$c'")
      case requiresEvaluation => print(s"not implemented YET for token $requiresEvaluation")
    }
    (ctx, "handled write construction")
  }

  private def prt(codeInside: String): Unit =
    getMain.insertAfter(s"System.out.println($codeInside);")
}

/*
final case class LIST(vals: List[Token[_]]) extends Token[List[Token[_]]] {
  override val scalaVal: List[Token[_]] = vals
}

final case class IF(cond: Token[_], positive: Token[_], negative: Token[_])
    extends Token[(Token[_], Token[_], Token[_])] {
  override val scalaVal = (cond, positive, negative)
}

final case class LAMBDA(vars: List[Token[_]], expr: Token[_]) extends Token[LambdaType] {
  override val scalaVal = (vars, expr)
}

final case class LISTCONS(elems: List[Token[_]]) extends Token[List[Token[_]]] {
  override val scalaVal = elems
}

final case class COND(elems: List[(Token[_], Token[_])]) extends Token[List[(Token[_], Token[_])]] {
  override val scalaVal = elems
}
 */
