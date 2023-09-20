// Copied from: https://gist.github.com/keynmol/06231181c08af18bc78f92a7d9426806
import io.circe.syntax._
import io.circe.Codec
import io.circe.Decoder
import io.circe.Encoder
import io.circe.derivation.Configuration

given Configuration = Configuration.default
  .withDiscriminator("kind")
  .withTransformConstructorNames {
    case "Integer" => "Int"
    case other     => other
  }

case class Loc(start: Int, end: Int, filename: String) derives Codec.AsObject

enum BinaryOp:
  case Concat, Add, Sub, Mul, Div, Rem, Eq, Neq, Lt, Gt, Lte, Gte, And, Or

object BinaryOp:
  given Codec[BinaryOp] =
    Codec.from(
      Decoder.decodeString.map(BinaryOp.valueOf),
      Encoder.encodeString.contramap(_.toString)
    )

case class Parameter(text: String, location: Loc) derives Codec.AsObject

enum Term:
  case Integer(value: Int, location: Loc)
  case Bool(value: Boolean, location: Loc)
  case Str(value: String, location: Loc)
  case Var(text: String, location: Loc)
  case Function(
      parameters: List[Parameter],
      value: Term,
      location: Loc
  )
  case Call(callee: Term, arguments: List[Term], location: Loc)
  case Binary(lhs: Term, op: BinaryOp, rhs: Term, location: Loc)
  case Let(name: Var, value: Term, next: Term, location: Loc)
  case If(condition: Term, `then`: Term, otherwise: Term, location: Loc)
  case Print(value: Term, location: Loc)

object Term:
  given Codec[Term] = Codec.AsObject.derivedConfigured

case class Program(name: String, expression: Term, location: Loc)
    derives Codec.AsObject
