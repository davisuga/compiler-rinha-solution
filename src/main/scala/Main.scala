package graalinterpreter
import io.circe.parser.decode
import ast._
import truffled.{_, given}

@main
def main =
  val filePath =
    os.pwd / os.up / "rinha-de-compilador" / "files" / "fib.json"
  val code = os.read(filePath)

  val program = (decode[Program](code))
  program match
    case Left(value) => println(value)
    case Right(Program(name, expr, _)) =>
      println(evalTerm(Map.empty, expr))
