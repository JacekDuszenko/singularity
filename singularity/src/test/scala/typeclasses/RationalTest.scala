package typeclasses

import org.scalatest.Reporter
import util.ParseSpec

class RationalTest extends ParseSpec {

  it should "use test reporter in a method without throwing an exception" in {
    def fun(rep: Reporter): Unit = {
      print(rep)
    }

    fun(MockTestReporter)

    // no exception thrown
  }

  it should "collect cmd opts from string interpolation, then move them to seq, then concatenate with space" in {
    val cmdOpts =
      s"""native-image
           |-H:ConfigurationFileDirectories=/cfgs
           |--no-fallback
           |--allow-incomplete-classpath
           |--no-server
           |--static
           |--enable-all-security-services
           |--report-unsupported-elements-at-runtime
           |-jar nameOfJarFile.jar
           |""".stripMargin
    val asSeq = cmdOpts.split("\n")
    asSeq.length shouldBe 9
    val asCmdLine = asSeq.mkString(" ")
    print(asCmdLine)
  }
}
