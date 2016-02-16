package syntax.atomize

import syntax.UsefulDataTypes.|
import syntax.processor.ProcessedAstNode

sealed trait Atom {
  val ast: Atom.Ast
}
object Atom {

  type Ast = ProcessedAstNode | Atom.Generated

  sealed class Generated
  object Generated extends Generated

  type Id = Value | LiteralGroup
  case class Value(value: String)(val ast: Ast) extends Atom
  case class LiteralGroup(literal: String, value: String)(val ast: Ast) extends Atom

  case class Val(name: Id, body: Expression)(val ast: Ast) extends Atom

  sealed trait Expression extends Atom
  case class Block(body: Seq[Val | Expression])(val ast: Ast) extends Expression
  case class Reference(to: Id)(val ast: Ast) extends Expression
  case class Function(argumentName: Id, argumentType: Expression, body: Expression)(val ast: Ast) extends Expression
  case class Application(target: Expression, argument: Expression)(val ast: Ast) extends Expression
  case class FunctionType(argumentType: Expression, returnType: Expression)(val ast: Ast) extends Expression
}
