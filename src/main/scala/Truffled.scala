package graalinterpreter.truffled

import graalinterpreter.ast
import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.nodes.{Node, NodeInfo, RootNode}
import com.oracle.truffle.api.CompilerDirectives
import graalinterpreter.ast.*
import com.oracle.truffle.api.dsl.{NodeChild, NodeChildren, NodeFactory}
import com.oracle.truffle.api.nodes.Node.Child
import graalinterpreter.Env
import com.oracle.truffle.api
import scala.collection.mutable.HashMap
import graalinterpreter.Value

abstract sealed class TermNode extends Node

case class IntegerNode(original: ast.Integer) extends TermNode:
  def executeInt(): Int = original.value
case class BoolNode(original: ast.Bool) extends TermNode:
  def executeBool(): Boolean = original.value

case class StrNode(original: ast.Str) extends TermNode:
  def executeStr(): String = original.value

case class PrintNode(value: ast.Term) extends TermNode:
  def executeVoid() = println(value)

given Conversion[ast.Term, TermNode] = (astNode: ast.Term) =>
  astNode match
    case Integer(value, location) =>
      IntegerNode(astNode.asInstanceOf[ast.Integer])
    case node @ Bool(value, location) => BoolNode(node)
    case node @ Str(value, location)  => StrNode(node)
    // case node @ Var(text, location)   => VarNode(node)
    // case Var(text, location) => VarNode(astNode.asInstanceOf[decoder.Var])
    // case Function(parameters, value, location) => FunctionNode(astNode.asInstanceOf[decoder.Function])
    // case Call(callee, arguments, location) => CallNode(astNode.asInstanceOf[decoder.Call])
    case Binary(lhs, op, rhs, _) => BinaryOpNode(lhs, op, rhs)
    // case Let(name, value, next, location) => LetNode(astNode.asInstanceOf[decoder.Let])
    // case If(condition, then, otherwise, location) => IfNode(astNode.asInstanceOf[decoder.If])
    case Print(value, location) =>
      PrintNode(value)

case class BinaryOpNode(
    @NodeChild lhs: TermNode,
    op: BinaryOp,
    @NodeChild rhs: TermNode
) extends TermNode:
  def execute() =
    (lhs, op, rhs) match
      case (lhs: IntegerNode, BinaryOp.Add, rhs: IntegerNode) =>
        (lhs.executeInt() + rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Sub, rhs: IntegerNode) =>
        (lhs.executeInt() - rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Mul, rhs: IntegerNode) =>
        (lhs.executeInt() * rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Div, rhs: IntegerNode) =>
        (lhs.executeInt() / rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Rem, rhs: IntegerNode) =>
        (lhs.executeInt() % rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Eq, rhs: IntegerNode) =>
        (lhs.executeInt() == rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Neq, rhs: IntegerNode) =>
        (lhs.executeInt() != rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Lt, rhs: IntegerNode) =>
        (lhs.executeInt() < rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Gt, rhs: IntegerNode) =>
        (lhs.executeInt() > rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Lte, rhs: IntegerNode) =>
        (lhs.executeInt() <= rhs.executeInt())
      case (lhs: IntegerNode, BinaryOp.Gte, rhs: IntegerNode) =>
        (lhs.executeInt() >= rhs.executeInt())
      case (lhs: BoolNode, BinaryOp.And, rhs: BoolNode) =>
        (lhs.executeBool() && rhs.executeBool())
      case (lhs: BoolNode, BinaryOp.Or, rhs: BoolNode) =>
        (lhs.executeBool() || rhs.executeBool())
      case (lhs: StrNode, BinaryOp.Concat, rhs: StrNode) =>
        (lhs.executeStr() + rhs.executeStr())
      case _ =>
        throw new Exception(
          s"Invalid binary operation: $lhs $op $rhs"
        )

// implicit class IfNode(original: decoder.If) extends TermNode:
//   def execute(): Term = original.condition original.`then`

implicit class VarNode(original: ast.Var) extends TermNode:
  def executeVar(): String = original.text

// @NodeInfo(shortName = "function")
// case class FunctionNode(parameters: List[Parameter], value: Term)
//     extends TermNode {
//   @CompilerDirectives.CompilationFinal
//   private var cachedValue: TermNode = _

//   def execute(): Term = {
//     if (cachedValue == null) {
//       CompilerDirectives.transferToInterpreterAndInvalidate()
//       cachedValue = decoder.Function(parameters, value)
//     }
//     cachedValue.execute()
//   }
// }

import com.oracle.truffle.api.nodes.RootNode

case class ProgramRoot(
    @Child private var exprNode: TermNode
) extends RootNode(GraalInterpreter()) {
  def execute(frame: VirtualFrame) = this.exprNode.getRootNode().execute(frame)
}
import com.oracle.truffle.api.TruffleLanguage

case class GraalInterpreter() extends TruffleLanguage[graalinterpreter.Env] {

  override protected def createContext(
      env: TruffleLanguage.Env
  ): graalinterpreter.Env = new HashMap[String, Value]()

}
