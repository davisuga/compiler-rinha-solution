package graalinterpreter
import io.circe.parser.decode
import ast._
import truffled.{_, given}
import java.util.Date
import scala.collection.mutable.HashMap

@main
def main =
  val filePath =
    os.pwd / os.up / "rinha-de-compilador" / "files" / "combination.json"
  val code = os.read(filePath)

  val program = (decode[Program](code))
  var metrics = Array[Long]()
  program match
    case Left(value) => println(value)
    case Right(Program(name, expr, _)) =>
      for
        _ <- 1 to 10
        start = Date()
        _ = println(evalTerm(HashMap.empty, expr))
        end = Date()
        _ = metrics = metrics.appended(end.getTime() - start.getTime())
      yield ()
      println(metrics.sum / metrics.length)
