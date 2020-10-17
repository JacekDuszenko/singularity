package typeclasses

import org.scalatest.Reporter
import org.scalatest.events.Event

object MockTestReporter extends Reporter {
  override def apply(event: Event): Unit = print(event.timeStamp)
}
