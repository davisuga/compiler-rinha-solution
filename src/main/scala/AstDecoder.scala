// Copied from: https://gist.github.com/keynmol/06231181c08af18bc78f92a7d9426806
package graalinterpreter.ast

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

case class Parameter(text: String, location: Option[Loc] = None)
    derives Codec.AsObject

sealed trait Term

case class Integer(value: Int, location: Option[Loc] = None) extends Term
case class Bool(value: Boolean, location: Option[Loc] = None) extends Term
case class Str(value: String, location: Option[Loc] = None) extends Term

case class Var(text: String, location: Option[Loc] = None) extends Term
case class Function(
    parameters: List[Parameter],
    value: Term,
    location: Option[Loc] = None
) extends Term
case class Call(
    callee: Term,
    arguments: List[Term],
    location: Option[Loc] = None
) extends Term
case class Binary(
    lhs: Term,
    op: BinaryOp,
    rhs: Term,
    location: Option[Loc] = None
) extends Term
case class Let(name: Var, value: Term, next: Term, location: Option[Loc] = None)
    extends Term
case class If(
    condition: Term,
    `then`: Term,
    otherwise: Term,
    location: Option[Loc] = None
) extends Term
case class Print(value: Term, location: Option[Loc] = None) extends Term
case class First(value: Term, location: Option[Loc] = None) extends Term
case class Second(value: Term, location: Option[Loc] = None) extends Term
case class Tuple(first: Term, second: Term, location: Option[Loc] = None)
    extends Term

object Term:
  given Codec[Term] = Codec.AsObject.derivedConfigured
case class Program(name: String, expression: Term, location: Option[Loc] = None)
    derives Codec.AsObject
