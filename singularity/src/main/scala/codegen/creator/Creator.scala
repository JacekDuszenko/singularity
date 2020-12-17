package codegen.creator

import codegen.Context

trait Creator {
  def handle: (Context, String)
}
