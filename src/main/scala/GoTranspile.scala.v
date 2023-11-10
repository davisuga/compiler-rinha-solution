package graalinterpreter.transpiler.go
import graalinterpreter.ast._

def transpile(term: Term): String = term match {
  case Integer(value) => value.toString
  case Bool(value)    => value.toString
  case Str(value)     => "\"" + value + "\""
  case Var(text)      => text
  case Function(parameters, value) =>
    "func(" + parameters.map(_.text).mkString(", ") + ") " + transpile(value)
  case Call(callee, arguments) =>
    transpile(callee) + "(" + arguments.map(transpile).mkString(", ") + ")"
  case Binary(lhs, op, rhs) =>
    transpile(lhs) + " " + transpile(op) + " " + transpile(rhs)
  case Let(name, value, next) =>
    "var " + name.text + " = " + transpile(value) + "\n" + transpile(next)
  case If(condition, _then, otherwise) =>
    "if " + transpile(condition) + " { " + transpile(
      _then
    ) + " } else { " + transpile(otherwise) + " }"
  case Print(value)  => "fmt.Println(" + transpile(value) + ")"
  case First(value)  => transpile(value) + ".first"
  case Second(value) => transpile(value) + ".second"
  case Tuple(first, second) =>
    "struct {first " + transpile(first) + "; second " + transpile(second) + "}"
  case Program(name, expression) =>
    "package main\n\nimport \"fmt\"\n\nfunc main() {\n" + transpile(
      expression
    ) + "\n}"
}

def transpile(op: BinaryOp): String = op match {
  case Concat => "+"
  case Add    => "+"
  case Sub    => "-"
  case Mul    => "*"
  case Div    => "/"
  case Rem    => "%"
  case Eq     => "=="
  case Neq    => "!="
  case Lt     => "<"
  case Gt     => ">"
  case Lte    => "<="
  case Gte    => ">="
  case And    => "&&"
  case Or     => "||"
}
