package graalinterpreter.unboxed
import graalinterpreter.ast._
import graalinterpreter.ast
import math.Numeric.Implicits.*
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import scala.collection.mutable.HashMap

case class Closure(parameters: List[Parameter], value: Term, context: Env)

type Env = HashMap[String, Value]

type ValuePrimitive = String | Int | Boolean | Closure
type Value = ValuePrimitive | (ValuePrimitive, ValuePrimitive)

extension (value: Value)
  def asInt = value match
    case value: Int => value
    case v          => throw new Error(s"Expected an int got ${v}")
  def asBoolean = value match
    case value: Boolean => value
    case v              => throw new Error(s"Expected a boolean got ${v}")
  def asClosure = value match
    case value: Closure => value
    case v              => throw new Error(s"Expected a closure got ${v}")

def evalBinary: (BinaryOp, Value, Value) => Value =
  case (BinaryOp.Concat, lhs: String, rhs: String) => (lhs + rhs)
  case (BinaryOp.Add, lhs: Int, rhs: Int)          => (lhs + rhs)
  case (BinaryOp.Sub, lhs: Int, rhs: Int)          => (lhs - rhs)
  case (BinaryOp.Mul, lhs: Int, rhs: Int)          => (lhs * rhs)
  case (BinaryOp.Div, lhs: Int, rhs: Int)          => (lhs / rhs)
  case (BinaryOp.Rem, lhs: Int, rhs: Int)          => (lhs % rhs)
  case (BinaryOp.Lt, lhs: Int, rhs: Int)           => (lhs < rhs)
  case (BinaryOp.Gt, lhs: Int, rhs: Int)           => (lhs > rhs)
  case (BinaryOp.Lte, lhs: Int, rhs: Int)          => (lhs <= rhs)
  case (BinaryOp.Gte, lhs: Int, rhs: Int)          => (lhs >= rhs)
  case (BinaryOp.Eq | BinaryOp.Neq, lhs, rhs)      => (lhs == rhs)
  case (BinaryOp.Or, lhs: Boolean, rhs: Boolean)   => (lhs || rhs)
  case (BinaryOp.And, lhs: Boolean, rhs: Boolean)  => (lhs && rhs)
  case (op, lhs, rhs) =>
    throw new Error(s"Invalid binary operation: $lhs $op $rhs")

def evalCall(context: Env, calleeTerm: Term, arguments: List[Term]) = {
  val callee = evalTerm(context, calleeTerm)
  val (Closure(parameters, value, _)) = callee.asClosure

  val newContext = parameters
    .zip(arguments)
    .map { case (parameter, argument) =>
      parameter.text -> evalTerm(context, argument)
    }
    .toMap
  evalTerm(context ++ newContext, value)
}

def memoize[I, O](f: I => O): I => O = new HashMap[I, O]() {
  override def apply(key: I) = getOrElseUpdate(key, f(key))
}

lazy val evalTerm: ((Env, Term)) => Value =
  memoize[(Env, Term), Value] { (context, term) =>
    term match
      case Integer(value, location) => (value)
      case Bool(value, location)    => (value)
      case Str(value, location)     => (value)

      case Var(text, location) =>
        context.getOrElse(text, throw new Error(s"Unknown variable $text"))
      case Function(parameters, value, location) =>
        (Closure(parameters, value, context))
      case Call(calleeTerm, arguments, location) =>
        evalCall(context, calleeTerm, arguments)
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
      // case Tuple(first, second, location) =>
      //   val fst: Value = evalTerm(context, first)
      //   val snd = evalTerm(context, second)
      //   (fst, snd)
      case Second(value, location) =>
        evalTerm(context, value) match
          case (first, second) => second
      case First(value, location) =>
        evalTerm(context, value) match
          case (first, second) => first

  }
