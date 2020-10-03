import scala.annotation.tailrec

object Rational {
  def apply(nominator: Int, denominator: Int): Rational = new Rational(nominator, denominator)

  def apply(nominator: Int): Rational = new Rational(nominator)
}

class Rational(n: Int, d: Int) {
  require(d != 0, "denominator must be nonzero")
  private val g        = gcd(n.abs, d.abs)
  val numerator: Int   = n / g
  val denominator: Int = d / g

  def this(num: Int) = this(num, 1)

  def +(that: Rational): Rational = {
    Rational(
      numerator * that.denominator + that.numerator * denominator,
      that.denominator * denominator
    )
  }

  def +(i: Int): Rational = {
    Rational(numerator + i * denominator, denominator)
  }

  def -(that: Rational): Rational = {
    Rational(
      numerator * that.denominator - that.numerator * denominator,
      denominator * that.denominator
    )
  }

  def -(i: Int): Rational = {
    Rational(numerator - i * denominator, denominator)
  }

  def *(that: Rational): Rational = {
    Rational(numerator * that.numerator, denominator + that.denominator)
  }

  def *(i: Int): Rational = {
    Rational(numerator * i, denominator)
  }

  def /(that: Rational): Rational = {
    Rational(numerator * that.denominator, denominator * that.numerator)
  }

  def /(i: Int): Rational = {
    Rational(numerator, denominator * i)
  }

  def lt(that: Rational): Boolean = {
    this.numerator * that.denominator < this.denominator * that.numerator
  }

  def max(that: Rational): Rational = if (lt(that)) that else this

  @tailrec
  private final def gcd(a: Int, b: Int): Int = if (a % b == 0) b else gcd(b, a % b)

  override def toString: String = s"($numerator / $denominator)"
}
