object Rational {
  def apply(nominator: Int, denominator: Int): Rational = new Rational(nominator, denominator)
}

class Rational(nominator: Int, denominator: Int) {
  println("created")
}