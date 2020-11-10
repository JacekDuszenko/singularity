import typeclasses.{GpuTask, ProcessorTask, Show}

object Main {

  implicit def tupleShower(
      implicit proctShow: Show[ProcessorTask],
      gputShow:           Show[GpuTask]
  ): Show[(ProcessorTask, GpuTask)] =
    (a: (ProcessorTask, GpuTask)) => proctShow.show(a._1) ++ "THATS SIMPLE" ++ gputShow.show(a._2)

  def showObj[A](a: A)(implicit shower: Show[A]): Unit = {
    println(shower.show(a))
  }

  def main(params: Array[String]): Unit = {

    val procT = ProcessorTask(13, "task 1", started = true)
    val gpuT  = GpuTask(30, 35)

    showObj(procT)
    showObj((procT, gpuT))
  }
}
