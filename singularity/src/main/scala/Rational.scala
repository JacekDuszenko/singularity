object Rational {
  def apply(nominator: Int, denominator: Int): Rational = new Rational(nominator, denominator)
}

class Rational(nominator: Int, denominator: Int) {
  require(denominator != 0, "denominator must be nonzero")
  print (this.toString)

  override def toString: String = s"($nominator / $denominator)"
}