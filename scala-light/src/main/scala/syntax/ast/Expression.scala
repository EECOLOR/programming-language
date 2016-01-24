package syntax.ast

import Shared._

sealed trait Expression
object Expression {

  case class Block(body: Seq[Statement | Expression]) extends Expression
  case class Function(arguments: Seq[Argument], body: Expression) extends Expression
  case class Application(target: Expression, argument: Expression) extends Expression
  case class MarkedLiteralGroup(mark: Indexed, literalGroup: LiteralGroup) extends Expression
  case class MemberAccess(expression: Expression, member: IdReference) extends Expression
  case class Product(expressions: Seq[Expression]) extends Expression
  case class ProductApplication(target: Expression, arguments: Seq[(Option[Id], Expression)]) extends Expression
  case class Reference(to: Shared.Reference) extends Expression
}
