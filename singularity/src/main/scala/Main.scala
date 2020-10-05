import typeclasses.Rational

object Main {

  def sum(a: Int, b: Int, c: Int, d: Int): Int = {
    a + b + c + d
  }

  def main(params: Array[String]): Unit = {

    val a = Rational(2)
    val c = 3 * a
    Seq(1, 2, 3).exists(_ % 2 == 1)
    val f  = sum _
    val f2 = sum(_, _, _, _)

    print(f2 == f)

  }
}
