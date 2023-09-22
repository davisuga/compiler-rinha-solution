package graalinterpreter
import io.circe.parser.decode
import ast._
import truffled.{_, given}
import java.util.Date
import scala.collection.mutable.HashMap
import scala.io.StdIn.readLine
import os.Path
import os.RelPath

@main
def main(
    args: String*
) =
  val argPath = args.headOption
    .map(RelPath(_))
    .map(_.resolveFrom(os.pwd))

  val officialPath =
    Path("/var/rinha/source.rinha.json")

  val code = os.read(argPath.getOrElse(officialPath))

  val program = (decode[Program](code))
  var metrics = Array[Long]()
  program match
    case Left(value) => println(value)
    case Right(Program(name, expr, _)) =>
      evalTerm(HashMap.empty, expr)
