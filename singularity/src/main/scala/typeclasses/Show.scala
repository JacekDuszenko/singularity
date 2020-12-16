package typeclasses

import scala.concurrent.duration.Duration

trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A](implicit s: Show[A]): Show[A] = s

  def instance[A](func: A => String): Show[A] = {
    new Show[A] {
      override def show(a: A) = func(a)
    }
  }

  implicit val intShow: Show[Int]      = instance((num: Int) => s"this is an int: $num")
  implicit val boolShow: Show[Boolean] = instance((b:   Boolean) => s"This bool has value: $b")
  implicit val durationShow: Show[Duration] = instance(
    (dura: Duration) => s"the duration is: ${dura.toSeconds} seconds"
  )
}
