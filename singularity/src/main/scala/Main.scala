import parser.SchemeParser

object Main {
  def main(params: Array[String]): Unit = {
    val res = SchemeParser.parse("#t")
    println(res)
  }
}
