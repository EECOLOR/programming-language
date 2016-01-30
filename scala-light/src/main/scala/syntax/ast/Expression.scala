package syntax.ast

import Shared._

sealed trait Expression
object Expression {

  case class Application(target: Expression, argument: Expression) extends Expression
  case class Block(body: Seq[Statement | Expression]) extends Expression
  case class BlockFunction(arguments: Seq[Argument], body: Expression) extends Expression
  case class Function(arguments: Seq[Argument], body: Expression) extends Expression
  case class MarkedLiteralGroup(mark: Indexed, literalGroup: LiteralGroup) extends Expression
  case class MemberAccess(target: Expression, member: IdReference) extends Expression
  case class NamedProductApplication(target: Expression, arguments: Seq[(Option[Id], Expression)]) extends Expression
  case class Product(expressions: Seq[Expression]) extends Expression
  case class ProductApplication(target: Expression, arguments: Seq[Expression]) extends Expression
  case class Reference(to: Shared.Reference) extends Expression
  case class WhitespaceApplication(target: Expression, method: IdReference, argument: Expression) extends Expression
}
