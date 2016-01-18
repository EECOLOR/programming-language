package syntax.ast

trait Expression
object Expression {

  import Core._

  case class Block(block: Core.Block) extends Expression
  case class BlockApplication(target: Expression, argument: Block) extends Expression
  case class Function(argument: Arguments | Typed[Id]| Product, body: Expression) extends Expression
  case class MemberAccess(expression: Expression, member: QualifiedReference) extends Expression
  case class Product(expressions: Seq[(Option[Id], Expression)]) extends Expression
  case class ProductApplication(target: Expression, arguments: Product) extends Expression
  case class Reference(to: Core.QualifiedReference) extends Expression
  case class WhitespaceApplication(target: Expression, method: Core.Reference, argument: Expression) extends Expression

  type |[+A, +B] = Either[A, B]
}
