import io.circe.parser.decode

@main
def main =
  val filePath = os.pwd / os.up / "files" / "combination.json"
  val code = os.read(filePath)

  println(decode[Program](code))
