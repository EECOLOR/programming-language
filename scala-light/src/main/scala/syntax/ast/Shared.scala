package syntax.ast

object Shared {
  type |[+A, +B] = Either[A, B]

  type Id = Value | LiteralGroup
  type Reference = Seq[IdReference]

  case class Argument(id: Id, `type`: Option[Expression], defaultValue: Option[Expression])(val position: Position) extends AstNode
  case class IdReference(to: Id, typeApplication: Seq[Expression])(val position: Position) extends AstNode
  case class Value(value: String)(val position: Position) extends AstNode
  case class LiteralGroup(literal: String, value: Value)(val position: Position) extends AstNode
}
