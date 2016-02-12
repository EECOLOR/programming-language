package syntax.processor

import syntax.UsefulDataTypes.|
import syntax.ast.AstNode

object Shared {
  type Id = Value | LiteralGroup
  case class Value(value: String)(val ast: AstNode) extends ProcessedAstNode
  case class LiteralGroup(literal: String, value: String)(val ast: AstNode) extends ProcessedAstNode

  // question: Should arguments have type parameters?
  case class Argument(name: Id, `type`: Option[Expression])(val ast: AstNode) extends ProcessedAstNode
}
