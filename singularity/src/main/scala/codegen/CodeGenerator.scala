package codegen

import cats.data.State
import cats.effect.IO
import codegen.TypeSystemBootstrapper.initializeTypeSystemBuilder
import codegen.creator.impl.{ReadCreator, WriteCreator}
import config.SETTINGS
import javassist.ClassPool
import model.{DEF, ID, READDEF, Token, WRITE}

final case class CodeGenerator(tks: List[Token[_]]) {

  def generateCode: IO[Unit] =
    for {
      _           <- logTokens
      codegenPlan <- buildState
      _           <- generate(codegenPlan)
    } yield ()

  def generate(codegenPlan: State[Context, String]): IO[Unit] =
    IO(codegenPlan.run(initContext).value._1)

  private def buildState: IO[State[Context, String]] =
    IO(
      tks
        .map(transformToState)
        .foldLeft(initializeTypeSystemBuilder)(
          (ss, aa) => ss.flatMap(_ => aa)
        )
        .flatMap(_ => finalizerState)
    )

  private def transformToState(tkn: Token[_]): State[Context, String] = State[Context, String] {
    context =>
      tkn match {
        case DEF(elem, expr)
        case READDEF(elem: ID) =>
          new ReadCreator(context, elem.scalaVal).handle
        case WRITE(expr) => WriteCreator(context, expr).handle
      }
  }

  private def finalizerState: State[Context, String] =
    State[Context, String] { ctx =>
      ctx.classnamesToWrite.foreach(
        c => ClassPool.getDefault.get(c).writeFile(SETTINGS.CompileOutputDir)
      )
      (ctx, s"written all generated bytecode to files in directory: ${SETTINGS.CompileOutputDir}")
    }

  private def logTokens = IO(println(tks))

  private def initContext: Context = Context(Map(), List())
}
