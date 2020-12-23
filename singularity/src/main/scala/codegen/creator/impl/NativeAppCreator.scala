package codegen.creator.impl

import codegen.CodegenHelpers.getMthd
import codegen.Context
import codegen.creator.Creator
import config.SETTINGS.NativeOperators
import model.Token

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
    val op = NativeOperators(varName)
    op.argLen match {
      case 2 =>
        val fst :: snd :: _ = tail
        val (c1, fstVar)    = evalArg(fst, ctx) //todo jak to zrobic
        val (c2, sndVar)    = evalArg(snd, c1)
        m.insertAfter(op.formatArgs(fstVar, sndVar))
        (c2, "applied native op")
    }
  }

  private def evalArg(tkn: Token[_], ctx: Context): (Context, String) = ???
  override def m = getMthd(execCls, execMtd)
}
