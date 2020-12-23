package codegen.creator

import codegen.CodegenHelpers.getMain
import codegen.Context
import javassist.CtMethod

trait Creator {
  def handle: (Context, String)
  def m: CtMethod = getMain
}
