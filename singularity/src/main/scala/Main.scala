import lexer.SchemeLexer
import typeclasses.Rational

object Main {

  def main(params: Array[String]): Unit = {

    def f(s: String) = s.reverse.dropWhile(_ != '/').drop(1).reverse

    val c = "/home/jacek/testowy/algorytm"
    print(f(c))
  }
}
