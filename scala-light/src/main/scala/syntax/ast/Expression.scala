package syntax.ast

import Shared._

sealed trait Expression
object Expression {

  case class Application(target: Expression, argument: Expression)(val position: Position) extends Expression
  case class Block(body: Seq[Statement | Expression])(val position: Position) extends Expression
  case class BlockFunction(arguments: Seq[Argument], body: Seq[Statement | Expression])(val position: Position) extends Expression
  case class Function(arguments: Seq[Argument], body: Expression)(val position: Position) extends Expression
  case class MarkedLiteralGroup(mark: Value, literalGroup: LiteralGroup)(val position: Position) extends Expression
  case class MemberAccess(target: Expression, member: IdReference)(val position: Position) extends Expression
  case class NamedProductApplication(target: Expression, arguments: Seq[(Option[Id], Expression)])(val position: Position) extends Expression
  case class Product(expressions: Seq[Expression])(val position: Position) extends Expression
  case class ProductApplication(target: Expression, arguments: Seq[Expression])(val position: Position) extends Expression
  case class Reference(to: Shared.Reference)(val position: Position) extends Expression
  case class WhitespaceApplication(target: Expression, method: IdReference, argument: Expression)(val position: Position) extends Expression
}
