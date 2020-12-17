import java.io.{File, FileInputStream}

import cats.effect.{ExitCode, IO, IOApp, Resource}
import codegen.CodeGenerator
import parser.SchemeParser

import scala.util.chaining.scalaUtilChainingOps

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _        <- validateArgs(args)
      codeFile <- getCodeFileReference(args)
      _ <- loadCodeFromFile(codeFile).use { fis =>
            fileContents(fis) pipe compile
          }
    } yield ExitCode.Success

  private def validateArgs(args: List[String]) = {
    if (args.length != 1) IO.raiseError(new RuntimeException("need to have only one arg"))
    else IO.unit
  }

  private def getCodeFileReference(args: List[String]) = IO(new File(args.head))

  private def fileContents(fis: FileInputStream) = new String(fis.readAllBytes())

  def compile(str: String): IO[Unit] =
    for {
      tokens <- IO.pure(SchemeParser.parse(str))
      _      <- CodeGenerator(tokens).generateCode
    } yield ()

  private def loadCodeFromFile(codeFile: File): Resource[IO, FileInputStream] = {
    Resource.make {
      IO(new FileInputStream(codeFile))
    } { fis =>
      IO(fis.close()).handleErrorWith(exc => IO(println(exc.getMessage)))
    }
  }
}
