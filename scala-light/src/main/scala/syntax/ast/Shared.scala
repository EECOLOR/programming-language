package syntax.ast

import syntax.UsefulDataTypes.|
import syntax.UsefulDataTypes.NonEmptySeq

object Shared {
  type Id = Value | LiteralGroup
  type Reference = NonEmptySeq[IdReference]

  case class Argument(id: Id, `type`: Option[Expression], defaultValue: Option[Expression])(val position: Position) extends AstNode
  case class IdReference(to: Id, typeApplication: Seq[Expression])(val position: Position) extends AstNode
  case class LiteralGroup(literal: String, value: Value)(val position: Position) extends AstNode
  case class Value(value: String)(val position: Position) extends AstNode
}
