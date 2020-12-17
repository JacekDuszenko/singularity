package codegen

import cats.effect.IO
import parser.Token

final case class CodeGenerator(tks: List[Token[_]]) {

  def generateCode: IO[Unit] =
    for {
      _ <- IO(println(tks))
    } yield ()
}
