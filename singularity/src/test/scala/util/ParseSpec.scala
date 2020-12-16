package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parser.SchemeParser

trait ParseSpec extends AnyFlatSpec with Matchers {
  val parser = SchemeParser
}
