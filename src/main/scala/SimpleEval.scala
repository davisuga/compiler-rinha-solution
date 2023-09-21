package graalinterpreter
import graalinterpreter.ast._
import graalinterpreter.ast

case class Closure(parameters: List[Parameter], value: Term, context: Env)

type Env = Map[String, Value]

enum Value:
  case IntV(value: Int)
  case BooleanV(value: Boolean)
  case StringV(value: String)
  case ClosureV(value: Closure)
import Value._

extension (value: Value)
  def asInt = value match
    case IntV(value) => value
    case v           => throw new Error(s"Expected an int got ${v}")
  def asBoolean = value match
    case BooleanV(value) => value
    case v               => throw new Error(s"Expected a boolean got ${v}")
  def asClosure = value match
    case ClosureV(value) => value
    case v               => throw new Error(s"Expected a closure got ${v}")

def evalBinary: (BinaryOp, Value, Value) => Value =
  case (BinaryOp.Concat, StringV(lhs), StringV(rhs)) => StringV(lhs + rhs)
  case (BinaryOp.Add, IntV(lhs), IntV(rhs))          => IntV(lhs + rhs)
  case (BinaryOp.Sub, IntV(lhs), IntV(rhs))          => IntV(lhs - rhs)
  case (BinaryOp.Mul, IntV(lhs), IntV(rhs))          => IntV(lhs * rhs)
  case (BinaryOp.Div, IntV(lhs), IntV(rhs))          => IntV(lhs / rhs)
  case (BinaryOp.Rem, IntV(lhs), IntV(rhs))          => IntV(lhs % rhs)
  case (BinaryOp.Lt, IntV(lhs), IntV(rhs))           => BooleanV(lhs < rhs)
  case (BinaryOp.Gt, IntV(lhs), IntV(rhs))           => BooleanV(lhs > rhs)
  case (BinaryOp.Lte, IntV(lhs), IntV(rhs))          => BooleanV(lhs <= rhs)
  case (BinaryOp.Gte, IntV(lhs), IntV(rhs))          => BooleanV(lhs >= rhs)
  case (BinaryOp.Eq | BinaryOp.Neq, lhs, rhs)        => BooleanV(lhs == rhs)
  case (BinaryOp.Or, BooleanV(lhs), BooleanV(rhs))   => BooleanV(lhs || rhs)
  case (BinaryOp.And, BooleanV(lhs), BooleanV(rhs))  => BooleanV(lhs && rhs)
  case (op, lhs, rhs) =>
    throw new Error(s"Invalid binary operation: $lhs $op $rhs")
object tailcalled:

  import scala.util.control.TailCalls._

  def evalTerm(context: Env, term: Term): TailRec[Value] =
    term match
      case Integer(value, location) => done(IntV(value))
      case Bool(value, location)    => done(BooleanV(value))
      case Str(value, location)     => done(StringV(value))
      case Var(text, location) =>
        done(
          context.getOrElse(text, throw new Error(s"Unknown variable $text"))
        )
      case Function(parameters, value, location) =>
        done(ClosureV(Closure(parameters, value, context)))
      case Call(calleeTerm, arguments, location) =>
        tailcall(evalTerm(context, calleeTerm)).flatMap { callee =>
          val args =
            arguments.map(arg => tailcall(evalTerm(context, arg)).result)
          val (Closure(parameters, value, ctx)) = callee.asClosure

          val newContext = parameters
            .zip(args)
            .map { case (parameter, argument) =>
              parameter.text -> argument
            }
            .toMap
          tailcall(evalTerm(context ++ newContext, value))
        }
      case Binary(lhs, op, rhs, location) =>
        for {
          lhsValue <- tailcall(evalTerm(context, lhs))
          rhsValue <- tailcall(evalTerm(context, rhs))
        } yield evalBinary(op, lhsValue, rhsValue)
      case Let(name, value, next, location) =>
        for {
          computedVal <- tailcall(evalTerm(context, value))
        } yield evalTerm(context + (name.text -> computedVal), next).result

      case If(condition, _then, otherwise, location) =>
        tailcall(evalTerm(context, condition)).flatMap { conditionValue =>
          if (conditionValue.asBoolean) tailcall(evalTerm(context, _then))
          else tailcall(evalTerm(context, otherwise))
        }
      case Print(value, location) =>
        tailcall(evalTerm(context, value)).map { result =>
          println(s"Stdout: $result")
          result
        }
def evalTerm(context: Env, term: Term): Value =
  term match
    case Integer(value, location) => IntV(value)
    case Bool(value, location)    => BooleanV(value)
    case Str(value, location)     => StringV(value)
    case Var(text, location) =>
      context.getOrElse(text, throw new Error(s"Unknown variable $text"))
    case Function(parameters, value, location) =>
      ClosureV(Closure(parameters, value, context))
    case Call(calleeTerm, arguments, location) =>
      val callee = evalTerm(context, calleeTerm)
      val args = arguments.map(evalTerm(context, _))
      val (Closure(parameters, value, ctx)) = callee.asClosure

      val newContext = parameters
        .zip(arguments)
        .map { case (parameter, argument) =>
          parameter.text -> evalTerm(context, argument)
        }
        .toMap
      evalTerm(context ++ newContext, value)
    case Binary(lhs, op, rhs, location) =>
      evalBinary(op, evalTerm(context, lhs), evalTerm(context, rhs))

    case Let(name, value, next, location) =>
      evalTerm(context + (name.text -> evalTerm(context, value)), next)

    case If(condition, _then, otherwise, location) =>
      if (evalTerm(context, condition).asBoolean) evalTerm(context, _then)
      else evalTerm(context, otherwise)
    case Print(value, location) =>
      val result = evalTerm(context, value)
      println(result)
      result
