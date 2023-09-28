package graalinterpreter
import ast._
import graalinterpreter.ast
import utils.memoize
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import graalinterpreter.utils.LRUCache

case class Closure(parameters: List[Parameter], value: Term, context: Env)

type Env = HashMap[String, Value]

enum Value:
  case IntV(value: Int)
  case BooleanV(value: Boolean)
  case StringV(value: String)
  case ClosureV(value: Closure)
  case TupleV(value: (Value, Value))
import Value._

trait PrettyPrintable[A]:
  extension (value: A) def pp: String

given PrettyPrintable[Value] with
  extension (value: Value)
    def pp: String = (value match
      case IntV(value)     => value
      case BooleanV(value) => value
      case StringV(value)  => value
      case ClosureV(_)     => "<#closure>"
      case TupleV((a, b))  => ((a).pp, (b).pp)
    ).toString

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
  case (BinaryOp.Add, lhs, rhs)                      => StringV(lhs.pp + rhs.pp)
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

case class Result(value: Value, printOutput: ListBuffer[String])

def evalCallBase(callValue: Call, context: Env): Result = {
  val Call(calleeTerm, arguments, location) = callValue
  val args = arguments.map(evalTerm(context, _))
  val (Closure(parameters, value, ctx)) =
    evalTerm(context, calleeTerm).asClosure

  val newContext = parameters
    .zip(arguments)
    .map { case (parameter, argument) =>
      parameter.text -> evalTerm(context, argument)
    }
    .toMap

  val printBuffer = new ListBuffer[String]()
  val result = evalTerm(context ++ newContext, value, printBuffer)
  Result(result, printBuffer)
}

val memoizedEvalCallCache =
  LRUCache[(Call, Env), Result](1000) // limit to 1000 entries

def memoizedEvalCall(call: (Call, Env)): Result = {
  memoizedEvalCallCache.get(call) match {
    case Some(result) => result
    case None =>
      val result = evalCallBase(call._1, call._2)
      memoizedEvalCallCache.put(call, result)
      result
  }
}
def evalTerm(
    context: Env,
    term: Term,
    printBuffer: ListBuffer[String] = new ListBuffer[String](),
    evalCall: ((Call, Env)) => Result = memoizedEvalCall
): Value =
  term match
    case Integer(value, location) => IntV(value)
    case Bool(value, location)    => BooleanV(value)
    case Str(value, location)     => StringV(value)
    case Var(text, location) =>
      context.getOrElse(text, throw new Error(s"Unknown variable $text"))
    case Function(parameters, value, location) =>
      ClosureV(Closure(parameters, value, context))
    case callValue @ Call(calleeTerm, arguments, location) =>
      val Result(value, printOutput) = evalCall(callValue, context)
      printOutput.foreach(println)
      value
    case Print(value, location) =>
      val result = evalTerm(context, value, printBuffer)
      val printOutput = (result).pp
      println(printOutput)
      printBuffer += printOutput.toString()
      result
    case Binary(lhs, op, rhs, location) =>
      evalBinary(op, evalTerm(context, lhs), evalTerm(context, rhs))

    case Let(name, value, next, location) =>
      evalTerm(context + (name.text -> evalTerm(context, value)), next)

    case If(condition, _then, otherwise, location) =>
      if (evalTerm(context, condition).asBoolean) evalTerm(context, _then)
      else evalTerm(context, otherwise)
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
