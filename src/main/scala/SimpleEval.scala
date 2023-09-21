package graalinterpreter
import graalinterpreter.ast._
import graalinterpreter.ast
import scala.collection.mutable.HashMap

case class Closure(parameters: List[Parameter], value: Term, context: Env)

type Env = HashMap[String, Value]

enum Value:
  case IntV(value: Int)
  case BooleanV(value: Boolean)
  case StringV(value: String)
  case ClosureV(value: Closure)
  case TupleV(value: (Value, Value))

def printValue: Value => Unit = {
  case IntV(value)     => println(value)
  case BooleanV(value) => println(value)
  case StringV(value)  => println(value)
  case ClosureV(value) => println("<#closure>")
  case TupleV((a, b))  => println((printValue(a), printValue(b)))
}
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
      printValue(result)
      result
    case Tuple(first, second, location) =>
      val fst: Value = evalTerm(context, first)
      val snd = evalTerm(context, second)
      TupleV(fst, snd)
    case First(value, location) =>
      evalTerm(context, value) match
        case TupleV(first, second) => first
        case _                     => throw new Error("Expected a tuple")
    case Second(value, location) =>
      evalTerm(context, value) match
        case TupleV(first, second) => second
        case _                     => throw new Error("Expected a tuple")
